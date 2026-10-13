/// <reference path="./lib/fresh.d.ts" />

/**
 * Source Control — a VS Code-style source control view for Fresh.
 *
 * One tab ("*Source Control*") with:
 *   - a toolbar row of clickable actions,
 *   - a tree on the left: every git repository found under the workspace
 *     root (the root itself and nested repos, `scanDepth` levels deep), each
 *     with its branch, ahead/behind counts and its changes grouped as
 *     Merge Changes / Staged Changes / Changes,
 *   - a live preview on the right: the selected file's diff with old/new
 *     line numbers, a group's per-file +/- summary, or a repo's recent log.
 *
 * Enter on a file opens a side-by-side diff tab (HEAD ↔ index for staged
 * changes, index ↔ working tree for unstaged ones, like VS Code): only the
 * changed regions with a few lines of context, the rest folded, modified
 * lines paired with their changed words highlighted, syntax-coloured.
 *
 * Keys in the list: s stage · u unstage · x discard · c commit · b branch ·
 * p pull · P push · f fetch · o open · r refresh · t list/tree · h hide clean
 * repos · ] [ next/prev repo · Tab preview · ? help · q close. In the preview
 * (Tab), s / u / x act on the hunk under the cursor and Enter opens that line.
 * Rows also carry clickable [ + ] / [ − ] stage buttons.
 *
 * Keys in the diff tab: n/p next/prev change · Enter expand ⋯ / open file ·
 * a whole file · s / u / x stage / unstage / discard the change · q close.
 *
 * Open it with the palette command "Source Control: Open".
 * Settings live under `plugins.source_control.settings` in config.json.
 */

const editor = getEditor();

// =============================================================================
// Settings
// =============================================================================

editor.defineConfigInteger("scanDepth", {
  default: 3,
  minimum: 0,
  maximum: 6,
  description: "How many directory levels below the workspace root are scanned for nested git repositories.",
});
editor.defineConfigBoolean("showCleanRepos", {
  default: true,
  description: "List repositories that have no changes.",
});
editor.defineConfigInteger("autoRefreshMs", {
  default: 3000,
  minimum: 0,
  maximum: 600000,
  description: "Refresh the Source Control view every N milliseconds while it is open (0 disables).",
});
editor.defineConfigBoolean("copilotCommitMessages", {
  default: true,
  description: "Let GitHub Copilot write a first draft of the commit message (needs the Copilot language server and a signed-in account, e.g. through the fresh-copilot plugin).",
});
editor.defineConfigBoolean("treeView", {
  default: false,
  description: "Show changed files as a folder tree instead of a flat list (the t key toggles it).",
});
editor.defineConfigInteger("diffContextLines", {
  default: 3,
  minimum: 0,
  maximum: 50,
  description: "Unchanged lines shown around each change in the side-by-side diff (the rest is folded).",
});
editor.defineConfigBoolean("nativeDiffView", {
  default: false,
  description: "Use Fresh's built-in composite view (whole file, no folding) instead of the compact side-by-side diff.",
});
editor.defineConfigInteger("previewMaxLines", {
  default: 5000,
  minimum: 100,
  maximum: 100000,
  description: "Maximum number of diff lines shown in the preview pane.",
});

interface Settings {
  scanDepth?: number;
  showCleanRepos?: boolean;
  autoRefreshMs?: number;
  previewMaxLines?: number;
  diffContextLines?: number;
  nativeDiffView?: boolean;
  treeView?: boolean;
  copilotCommitMessages?: boolean;
}

function settings(): Settings {
  return (editor.getPluginConfig() ?? {}) as Settings;
}

// =============================================================================
// Model
// =============================================================================

type Group = "conflict" | "staged" | "unstaged";

const GROUP_ORDER: Group[] = ["conflict", "staged", "unstaged"];
const GROUP_TITLE: Record<Group, string> = {
  conflict: "Merge Changes",
  staged: "Staged Changes",
  unstaged: "Changes",
};

interface Change {
  /** Repo-relative path. */
  path: string;
  /** Source path of a rename/copy. */
  origPath: string | null;
  /** Display letter: M A D R C T U (untracked) ! (conflict). */
  code: string;
  group: Group;
  untracked: boolean;
}

interface Repo {
  root: string;
  /** Path relative to the workspace root ("." shows the root's name). */
  label: string;
  branch: string;
  oid: string;
  upstream: string | null;
  ahead: number;
  behind: number;
  changes: Change[];
  error: string | null;
}

/** `parents` are the keys of the rows above this one in the tree, outermost
 *  first; a row is hidden when any of them is collapsed. */
type Row =
  | { kind: "repo"; key: string; repo: Repo; parents: string[] }
  | { kind: "group"; key: string; repo: Repo; group: Group; changes: Change[]; parents: string[] }
  | {
    kind: "dir";
    key: string;
    repo: Repo;
    group: Group;
    /** Repo-relative directory the row stands for. */
    path: string;
    /** What is shown: single-child folder chains are compacted ("src/util"). */
    label: string;
    changes: Change[];
    depth: number;
    parents: string[];
  }
  | { kind: "file"; key: string; repo: Repo; change: Change; depth: number; parents: string[] }
  | { kind: "info"; key: string; repo: Repo | null; text: string; depth: number; parents: string[] };

/** Rows listed per group; the rest are summarised (a forgotten build dir
 *  can hold tens of thousands of untracked files). */
const MAX_GROUP_FILES = 1000;

const repoKey = (r: Repo) => `R:${r.root}`;
const groupKey = (r: Repo, g: Group) => `G:${r.root}:${g}`;
const fileKey = (r: Repo, c: Change) => `F:${r.root}:${c.group}:${c.path}`;

// =============================================================================
// State
// =============================================================================

const TREE_PANEL = 1;
const TOOLBAR_PANEL = 2;
const TREE_KEY = "scm-tree";

interface PreviewDiff {
  repo: Repo;
  change: Change;
  /** `diff --git` … `+++` lines of the file's patch. */
  header: string;
  /** Each hunk's text, from its `@@` line. */
  hunks: string[];
}

interface CompositeState {
  compositeId: number;
  oldId: number;
  newId: number;
  absPath: string;
}

const state = {
  open: false,
  groupId: null as number | null,
  treeBufferId: null as number | null,
  toolbarBufferId: null as number | null,
  detailBufferId: null as number | null,
  treeMounted: false,
  toolbarMounted: false,
  workspace: "",
  roots: [] as string[],
  repos: [] as Repo[],
  rows: [] as Row[],
  selectedKey: null as string | null,
  selectedIndex: 0,
  collapsed: new Set<string>(),
  signature: "",
  refreshing: false,
  refreshPending: false,
  timer: null as number | null,
  ticks: 0,
  previewToken: 0,
  /** The file diff currently in the preview, split for hunk operations. */
  previewDiff: null as PreviewDiff | null,
  /** Session override of `showCleanRepos` (the `h` key). */
  hideClean: null as boolean | null,
  /** Session override of `treeView` (the `t` key). */
  treeView: null as boolean | null,
  composite: null as CompositeState | null,
  busy: false,
};

// =============================================================================
// Git helpers
// =============================================================================

/** Settings that reshape `git diff` output, pinned back to the defaults the
 *  parser expects (same set the bundled git plugins pin). */
const DIFF_CONFIG = [
  "--no-optional-locks",
  "-c", "diff.noprefix=false",
  "-c", "diff.mnemonicPrefix=false",
  "-c", "core.quotepath=false",
  "-c", "diff.suppressBlankEmpty=false",
  "-c", "diff.relative=false",
  "-c", "diff.submodule=short",
  "-c", "core.bigFileThreshold=1m",
];

function diffArgs(...args: string[]): string[] {
  return [...DIFF_CONFIG, "diff", "--no-ext-diff", "--no-textconv", "--no-color", ...args];
}

function git(root: string, args: string[]): ProcessHandle<SpawnResult> {
  return editor.spawnProcess("git", args, root);
}

/** For network commands: never let git or ssh ask for a password on the
 *  terminal Fresh is drawing on. */
async function gitNetwork(root: string, args: string[]): Promise<SpawnResult> {
  const env = ["GIT_TERMINAL_PROMPT=0"];
  const ssh = await git(root, ["config", "--get", "core.sshCommand"]);
  if (ssh.exit_code !== 0 || !ssh.stdout.trim()) env.push("GIT_SSH_COMMAND=ssh -o BatchMode=yes");
  return await editor.spawnProcess("env", [...env, "git", ...args], root);
}

function firstLine(s: string): string {
  const lines = s.split("\n").map((l) => l.trim()).filter((l) => l.length > 0);
  return lines[0] ?? "";
}

function lastLine(s: string): string {
  const lines = s.split("\n").map((l) => l.trim()).filter((l) => l.length > 0);
  return lines[lines.length - 1] ?? "";
}

// =============================================================================
// Repository discovery
// =============================================================================

const SKIP_DIRS = new Set(["node_modules", "target", "__pycache__", "venv"]);

function scanForRepos(dir: string, depth: number, out: string[]): void {
  if (depth <= 0) return;
  let entries: DirEntry[];
  try {
    entries = editor.readDir(editor.authorityPath(dir));
  } catch {
    return;
  }
  for (const e of entries) {
    if (!e.is_dir || e.name.startsWith(".") || SKIP_DIRS.has(e.name)) continue;
    const sub = editor.pathJoin(dir, e.name);
    if (editor.fileExists(editor.authorityPath(editor.pathJoin(sub, ".git")))) {
      out.push(sub);
    } else {
      scanForRepos(sub, depth - 1, out);
    }
  }
}

async function discoverRepos(ws: string): Promise<string[]> {
  const roots: string[] = [];
  const top = await git(ws, ["rev-parse", "--show-toplevel"]);
  if (top.exit_code === 0 && top.stdout.trim()) roots.push(top.stdout.trim());
  const depth = settings().scanDepth ?? 3;
  const nested: string[] = [];
  scanForRepos(ws, depth, nested);
  // Resolve each candidate to its real toplevel (symlinks, worktrees).
  const resolved = await Promise.all(
    nested.map(async (dir) => {
      const r = await git(dir, ["rev-parse", "--show-toplevel"]);
      return r.exit_code === 0 ? r.stdout.trim() : "";
    }),
  );
  const extra = resolved.filter((r, i) => r && !roots.includes(r) && resolved.indexOf(r) === i);
  extra.sort((a, b) => a.localeCompare(b));
  return roots.concat(extra);
}

function labelFor(root: string): string {
  const ws = state.workspace.replace(/\/+$/, "");
  if (root === ws) return editor.pathBasename(root) || root;
  if (root.startsWith(ws + "/")) return root.slice(ws.length + 1);
  return root;
}

// =============================================================================
// Status
// =============================================================================

/** Split `s` on spaces into at most `n` fields (the last keeps the rest). */
function splitN(s: string, n: number): string[] {
  const out: string[] = [];
  let pos = 0;
  for (let k = 0; k < n - 1; k++) {
    const sp = s.indexOf(" ", pos);
    if (sp < 0) break;
    out.push(s.slice(pos, sp));
    pos = sp + 1;
  }
  out.push(s.slice(pos));
  return out;
}

function parseStatus(out: string, repo: Repo): void {
  const recs = out.split("\0");
  const add = (path: string, origPath: string | null, code: string, group: Group, untracked = false) =>
    repo.changes.push({ path, origPath, code, group, untracked });
  for (let i = 0; i < recs.length; i++) {
    const rec = recs[i];
    if (!rec) continue;
    const t = rec[0];
    if (t === "#") {
      const [, key, ...rest] = rec.split(" ");
      const val = rest.join(" ");
      if (key === "branch.head") repo.branch = val;
      else if (key === "branch.oid") repo.oid = val;
      else if (key === "branch.upstream") repo.upstream = val;
      else if (key === "branch.ab") {
        const m = val.match(/\+(\d+) -(\d+)/);
        if (m) {
          repo.ahead = parseInt(m[1]);
          repo.behind = parseInt(m[2]);
        }
      }
    } else if (t === "1" || t === "2") {
      const f = splitN(rec, t === "1" ? 9 : 10);
      const xy = f[1];
      const path = f[f.length - 1];
      const orig = t === "2" ? (recs[++i] ?? null) : null;
      if (xy[0] !== ".") add(path, orig, xy[0], "staged");
      if (xy[1] !== ".") add(path, null, xy[1], "unstaged");
    } else if (t === "u") {
      const f = splitN(rec, 11);
      add(f[10], null, "!", "conflict");
    } else if (t === "?") {
      add(rec.slice(2), null, "U", "unstaged", true);
    }
  }
}

async function loadRepo(root: string): Promise<Repo> {
  const repo: Repo = {
    root,
    label: labelFor(root),
    branch: "",
    oid: "",
    upstream: null,
    ahead: 0,
    behind: 0,
    changes: [],
    error: null,
  };
  const r = await git(root, [
    "--no-optional-locks",
    "-c", "core.quotepath=false",
    "status", "--porcelain=v2", "--branch", "-z", "--untracked-files=all",
  ]);
  if (r.exit_code !== 0) {
    repo.error = firstLine(r.stderr) || "git status failed";
    return repo;
  }
  parseStatus(r.stdout, repo);
  repo.changes.sort((a, b) => a.path.localeCompare(b.path));
  return repo;
}

// =============================================================================
// Rendering helpers
// =============================================================================

type Style = Partial<OverlayOptions>;
type Part = [string, Style?];

const C = {
  dim: "editor.line_number_fg",
  added: "ui.file_status_added_fg",
  modified: "ui.file_status_modified_fg",
  deleted: "ui.file_status_deleted_fg",
  renamed: "ui.file_status_renamed_fg",
  untracked: "ui.file_status_untracked_fg",
  conflicted: "ui.file_status_conflicted_fg",
  hunk: "syntax.keyword",
  branch: "syntax.function",
  header: "syntax.type",
};

function codeColor(code: string): string {
  switch (code) {
    case "A": return C.added;
    case "D": return C.deleted;
    case "R":
    case "C": return C.renamed;
    case "U": return C.untracked;
    case "!": return C.conflicted;
    default: return C.modified;
  }
}

/** Build a styled entry from parts; overlay offsets are in chars. */
function entry(parts: Part[], properties?: Record<string, unknown>): TextPropertyEntry {
  let text = "";
  const inlineOverlays: InlineOverlay[] = [];
  for (const [t, style] of parts) {
    const start = text.length;
    text += t;
    if (style && t.length > 0) inlineOverlays.push({ start, end: text.length, style, unit: "char" });
  }
  const e: TextPropertyEntry = { text, inlineOverlays };
  if (properties) e.properties = properties;
  return e;
}

function line(parts: Part[], properties?: Record<string, unknown>): TextPropertyEntry {
  return entry([...parts, ["\n"]], properties);
}

// =============================================================================
// Tree
// =============================================================================

function treeViewOn(): boolean {
  return state.treeView !== null ? state.treeView : settings().treeView === true;
}

interface DirNode {
  name: string;
  path: string;
  dirs: Map<string, DirNode>;
  files: Change[];
  /** Every change at or below this folder. */
  all: Change[];
}

/** Folder rows and file rows for one group, VS Code's "View as Tree":
 *  folders first, then files, alphabetically; a folder holding nothing but
 *  one sub-folder is merged into it. */
function pushTree(rows: Row[], repo: Repo, g: Group, changes: Change[], parents: string[]): void {
  const root: DirNode = { name: "", path: "", dirs: new Map(), files: [], all: [] };
  for (const c of changes) {
    const parts = c.path.split("/");
    let node = root;
    node.all.push(c);
    for (let i = 0; i < parts.length - 1; i++) {
      let child = node.dirs.get(parts[i]);
      if (!child) {
        child = { name: parts[i], path: node.path ? `${node.path}/${parts[i]}` : parts[i], dirs: new Map(), files: [], all: [] };
        node.dirs.set(parts[i], child);
      }
      child.all.push(c);
      node = child;
    }
    node.files.push(c);
  }
  const emit = (node: DirNode, depth: number, up: string[]) => {
    const dirs = [...node.dirs.values()].sort((a, b) => a.name.localeCompare(b.name));
    for (const d of dirs) {
      let cur = d;
      let label = d.name;
      while (cur.files.length === 0 && cur.dirs.size === 1) {
        cur = [...cur.dirs.values()][0];
        label += "/" + cur.name;
      }
      const key = `D:${repo.root}:${g}:${cur.path}`;
      rows.push({ kind: "dir", key, repo, group: g, path: cur.path, label, changes: cur.all, depth, parents: up });
      emit(cur, depth + 1, [...up, key]);
    }
    const files = [...node.files].sort((a, b) => a.path.localeCompare(b.path));
    for (const c of files) rows.push({ kind: "file", key: fileKey(repo, c), repo, change: c, depth, parents: up });
  };
  emit(root, 2, parents);
}

function buildRows(): Row[] {
  const rows: Row[] = [];
  const showClean = state.hideClean !== null ? !state.hideClean : settings().showCleanRepos !== false;
  if (state.repos.length === 0) {
    rows.push({ kind: "info", key: "I:none", repo: null, text: `No git repositories under ${state.workspace}`, depth: 0, parents: [] });
    return rows;
  }
  const tree = treeViewOn();
  for (const repo of state.repos) {
    if (!showClean && repo.changes.length === 0 && !repo.error) continue;
    const rk = repoKey(repo);
    rows.push({ kind: "repo", key: rk, repo, parents: [] });
    if (repo.error) {
      rows.push({ kind: "info", key: `I:${repo.root}`, repo, text: repo.error, depth: 1, parents: [rk] });
      continue;
    }
    for (const g of GROUP_ORDER) {
      const changes = repo.changes.filter((c) => c.group === g);
      if (changes.length === 0) continue;
      const gk = groupKey(repo, g);
      rows.push({ kind: "group", key: gk, repo, group: g, changes, parents: [rk] });
      const shown = changes.slice(0, MAX_GROUP_FILES);
      if (tree) {
        pushTree(rows, repo, g, shown, [rk, gk]);
      } else {
        for (const c of shown) rows.push({ kind: "file", key: fileKey(repo, c), repo, change: c, depth: 2, parents: [rk, gk] });
      }
      if (changes.length > MAX_GROUP_FILES) {
        rows.push({
          kind: "info",
          key: `I:${repo.root}:${g}:more`,
          repo,
          text: `… ${changes.length - MAX_GROUP_FILES} more (add them to .gitignore?)`,
          depth: 2,
          parents: [rk, gk],
        });
      }
    }
  }
  return rows;
}

function repoNodeText(repo: Repo): TextPropertyEntry {
  const parts: Part[] = [[repo.label, { bold: true }]];
  if (repo.branch) {
    const detached = repo.branch === "(detached)";
    const name = detached ? `detached@${repo.oid.slice(0, 7)}` : repo.branch;
    parts.push(["  ⎇ " + name, { fg: C.branch }]);
  }
  if (repo.ahead) parts.push([` ↑${repo.ahead}`, { fg: C.added }]);
  if (repo.behind) parts.push([` ↓${repo.behind}`, { fg: C.deleted }]);
  if (!repo.error && repo.upstream === null && repo.branch && repo.branch !== "(detached)") {
    parts.push([" (no upstream)", { fg: C.dim, italic: true }]);
  }
  const n = repo.changes.length;
  if (repo.error) parts.push(["  error", { fg: C.deleted }]);
  else if (n === 0) parts.push(["  ✓", { fg: C.dim }]);
  else parts.push([`  ${n}`, { fg: C.modified, bold: true }]);
  return entry(parts);
}

function fileNodeText(c: Change): TextPropertyEntry {
  const slash = c.path.lastIndexOf("/");
  const name = slash >= 0 ? c.path.slice(slash + 1) : c.path;
  const dir = slash >= 0 ? c.path.slice(0, slash) : "";
  const parts: Part[] = [
    [c.code + " ", { fg: codeColor(c.code), bold: true }],
    [name, c.code === "D" ? { strikethrough: true } : undefined],
  ];
  if (c.origPath) parts.push([` ← ${c.origPath}`, { fg: C.dim }]);
  if (dir && !treeViewOn()) parts.push([`  ${dir}`, { fg: C.dim }]);
  return entry(parts);
}

function treeSpec(): WidgetSpec {
  const nodes: TreeNode[] = state.rows.map((row) => {
    switch (row.kind) {
      case "repo":
        return {
          text: repoNodeText(row.repo),
          depth: 0,
          hasChildren: row.repo.changes.length > 0 || row.repo.error !== null,
        };
      case "group":
        return {
          text: entry([
            [GROUP_TITLE[row.group], { bold: true }],
            [`  ${row.changes.length}`, { fg: C.dim }],
          ]),
          depth: 1,
          hasChildren: true,
          action: row.group === "staged" ? "−" : "+",
        };
      case "dir":
        return {
          text: entry([
            [row.label, { fg: C.header }],
            [`  ${row.changes.length}`, { fg: C.dim }],
          ]),
          depth: row.depth,
          hasChildren: true,
          action: row.group === "staged" ? "−" : "+",
        };
      case "file":
        return {
          text: fileNodeText(row.change),
          depth: row.depth,
          hasChildren: false,
          action: row.change.group === "staged" ? "−" : "+",
        };
      case "info":
        return {
          text: entry([[row.text, { fg: C.dim, italic: true }]]),
          depth: row.depth,
          hasChildren: false,
        };
    }
  });
  return {
    kind: "tree",
    key: TREE_KEY,
    nodes,
    itemKeys: state.rows.map((r) => r.key),
    selectedIndex: state.selectedIndex,
    expandedKeys: expandedKeys(),
    checkable: false,
    itemHeight: 1,
    cardBorders: false,
    toggleOnClick: false,
    indentCols: 2,
  } as WidgetSpec;
}

function expandedKeys(): string[] {
  return state.rows
    .filter((r) => (r.kind === "repo" || r.kind === "group" || r.kind === "dir") && !state.collapsed.has(r.key))
    .map((r) => r.key);
}

/** Where the selection goes when its row disappeared (a file was staged,
 *  discarded, committed): the next surviving row of the same repo, else the
 *  closest surviving row above it. */
function fallbackIndex(oldRows: Row[], oldIdx: number): number {
  const at = new Map<string, number>();
  state.rows.forEach((r, i) => at.set(r.key, i));
  const root = oldRows[oldIdx]?.repo?.root;
  for (let i = oldIdx + 1; i < oldRows.length; i++) {
    if (oldRows[i].repo?.root !== root) break;
    const j = at.get(oldRows[i].key);
    if (j !== undefined && oldRows[i].kind === "file") return j;
  }
  for (let i = Math.min(oldIdx, oldRows.length) - 1; i >= 0; i--) {
    const j = at.get(oldRows[i].key);
    if (j !== undefined) return j;
  }
  return Math.min(oldIdx, state.rows.length - 1);
}

function renderTree(): void {
  if (state.treeBufferId === null) return;
  const oldRows = state.rows;
  state.rows = buildRows();
  let idx = state.selectedKey ? state.rows.findIndex((r) => r.key === state.selectedKey) : -1;
  if (idx < 0) idx = fallbackIndex(oldRows, state.selectedIndex);
  // Never land on a row hidden inside a collapsed parent.
  idx = Math.max(0, visibleAncestor(idx));
  state.selectedIndex = idx;
  state.selectedKey = state.rows[idx]?.key ?? null;
  const spec = treeSpec();
  if (!state.treeMounted) {
    editor.mountWidgetPanel(TREE_PANEL, state.treeBufferId, spec);
    state.treeMounted = true;
  } else {
    editor.updateWidgetPanel(TREE_PANEL, spec);
  }
  editor.widgetMutate(TREE_PANEL, { kind: "setExpandedKeys", widgetKey: TREE_KEY, keys: expandedKeys() } as WidgetMutation);
  editor.widgetMutate(TREE_PANEL, { kind: "setSelectedIndex", widgetKey: TREE_KEY, index: idx } as WidgetMutation);
}

/** The row itself, or its closest ancestor that is not inside a collapsed node. */
function visibleAncestor(idx: number): number {
  const row = state.rows[idx];
  if (!row) return idx;
  const hidden = row.parents.find((k) => state.collapsed.has(k));
  return hidden === undefined ? idx : state.rows.findIndex((r) => r.key === hidden);
}

// =============================================================================
// Toolbar
// =============================================================================

interface ToolbarItem {
  key: string;
  label: string;
  run: () => void | Promise<void>;
}

function toolbarItems(): ToolbarItem[] {
  return [
    { key: "stage", label: "s stage", run: scm_stage },
    { key: "unstage", label: "u unstage", run: scm_unstage },
    { key: "discard", label: "x discard", run: scm_discard },
    { key: "commit", label: "c commit", run: scm_commit },
    { key: "branch", label: "b branch", run: scm_branch },
    { key: "pull", label: "p pull", run: scm_pull },
    { key: "push", label: "P push", run: scm_push },
    { key: "tree", label: treeViewOn() ? "t list" : "t tree", run: scm_toggle_tree },
    { key: "refresh", label: "r refresh", run: scm_refresh },
    { key: "help", label: "? help", run: scm_help },
    { key: "close", label: "q close", run: scm_close },
  ];
}

function renderToolbar(): void {
  if (state.toolbarBufferId === null) return;
  const spec: WidgetSpec = {
    kind: "row",
    wrap: false,
    justifyEnd: false,
    children: [
      ...toolbarItems().map((it) => ({
        kind: "button",
        key: `tb.${it.key}`,
        label: it.label,
        focused: false,
        intent: "normal",
        disabled: false,
        focusable: false,
        bare: false,
        fullWidth: false,
      })),
      { kind: "spacer", cols: 0, flex: true },
    ],
  } as unknown as WidgetSpec;
  if (!state.toolbarMounted) {
    editor.mountWidgetPanel(TOOLBAR_PANEL, state.toolbarBufferId, spec, { autoFocusFirst: false });
    state.toolbarMounted = true;
  } else {
    editor.updateWidgetPanel(TOOLBAR_PANEL, spec);
  }
}

// =============================================================================
// Preview pane
// =============================================================================

function setDetail(entries: TextPropertyEntry[]): void {
  if (state.groupId === null) return;
  editor.setPanelContent(state.groupId, "detail", entries);
}

/** Render a unified diff with old/new line-number columns. */
function renderUnifiedDiff(out: string, maxLines: number): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];
  const lines = out.split("\n");
  if (lines.length && lines[lines.length - 1] === "") lines.pop();
  let oldNo = 0;
  let newNo = 0;
  let inHunk = false;
  let shown = 0;
  let hunk = -1;
  const W = 5;
  const num = (n: number | null) => (n === null ? "" : String(n)).padStart(W);
  for (const l of lines) {
    if (shown >= maxLines) {
      entries.push(line([[`… diff truncated at ${maxLines} lines (previewMaxLines)`, { fg: C.dim, italic: true }]]));
      break;
    }
    if (l.startsWith("@@")) {
      const m = l.match(/^@@+ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@+(.*)$/);
      if (m) {
        oldNo = parseInt(m[1]);
        newNo = parseInt(m[2]);
      }
      inHunk = true;
      hunk++;
      if (shown > 0) entries.push(line([[""]]));
      entries.push(line([[l, { fg: C.hunk, bold: true }]], { hunk, newLine: newNo }));
      shown++;
      continue;
    }
    if (!inHunk) {
      if (l.startsWith("Binary files")) {
        entries.push(line([["Binary file (or larger than 1 MiB) — no text diff", { fg: C.dim, italic: true }]]));
        shown++;
      } else if (/^(new file|deleted file|old mode|new mode|similarity|rename from|rename to)/.test(l)) {
        entries.push(line([[l, { fg: C.dim, italic: true }]]));
        shown++;
      }
      continue;
    }
    if (l.startsWith("diff --git")) {
      inHunk = false;
      continue;
    }
    const c = l[0];
    const props = { hunk, newLine: newNo };
    if (c === "+") {
      entries.push(line([[num(null) + " " + num(newNo) + " │", { fg: C.dim }], ["+" + l.slice(1), { fg: C.added }]], props));
      newNo++;
    } else if (c === "-") {
      entries.push(line([[num(oldNo) + " " + num(null) + " │", { fg: C.dim }], ["-" + l.slice(1), { fg: C.deleted }]], props));
      oldNo++;
    } else if (c === "\\") {
      entries.push(line([[" ".repeat(W * 2 + 1) + " │", { fg: C.dim }], [l, { fg: C.dim, italic: true }]], props));
    } else {
      entries.push(line([[num(oldNo) + " " + num(newNo) + " │", { fg: C.dim }], [" " + l.slice(1)]], props));
      oldNo++;
      newNo++;
    }
    shown++;
  }
  return entries;
}

function countDiff(out: string): { add: number; del: number } {
  let add = 0;
  let del = 0;
  let inHunk = false;
  for (const l of out.split("\n")) {
    if (l.startsWith("@@")) inHunk = true;
    else if (l.startsWith("diff --git")) inHunk = false;
    else if (inHunk && l[0] === "+") add++;
    else if (inHunk && l[0] === "-") del++;
  }
  return { add, del };
}

async function fileDiffText(repo: Repo, c: Change): Promise<string> {
  let r: SpawnResult;
  if (c.untracked) {
    r = await git(repo.root, diffArgs("--no-index", "--", "/dev/null", c.path));
  } else if (c.group === "staged") {
    const paths = c.origPath ? [c.origPath, c.path] : [c.path];
    r = await git(repo.root, diffArgs("--cached", "-M", "--", ...paths));
  } else if (c.group === "conflict") {
    // Two-way against HEAD: the conflict markers show up as plain additions,
    // which reads better than git's combined (`@@@`) format.
    r = await git(repo.root, diffArgs("HEAD", "--", c.path));
  } else {
    r = await git(repo.root, diffArgs("--", c.path));
  }
  return r.stdout;
}

async function previewFile(repo: Repo, c: Change, token: number): Promise<void> {
  const out = await fileDiffText(repo, c);
  if (token !== state.previewToken) return;
  state.previewDiff = splitPatch(repo, c, out);
  const { add, del } = countDiff(out);
  const what =
    c.group === "staged" ? "staged  (HEAD ↔ index)"
    : c.group === "conflict" ? "merge conflict"
    : c.untracked ? "untracked file"
    : "working tree  (index ↔ disk)";
  const entries: TextPropertyEntry[] = [
    line([
      [c.code + " ", { fg: codeColor(c.code), bold: true }],
      [c.path, { bold: true }],
      ...(c.origPath ? [[` ← ${c.origPath}`, { fg: C.dim }] as Part] : []),
    ]),
    line([
      [`${repo.label} · ${what}   `, { fg: C.dim }],
      [`+${add}`, { fg: C.added, bold: true }],
      [" "],
      [`-${del}`, { fg: C.deleted, bold: true }],
      ["   Enter: side-by-side · o: open file", { fg: C.dim, italic: true }],
    ]),
    line([[
      c.group === "conflict"
        ? "Resolve it in the file (o), then s marks it resolved"
        : c.untracked || c.origPath
        ? ""
        : c.group === "staged"
        ? "Tab here, then on a hunk: u unstage hunk · Enter open file at line"
        : "Tab here, then on a hunk: s stage hunk · x discard hunk · Enter open file at line",
      { fg: C.dim, italic: true },
    ]]),
    line([[""]]),
  ];
  const body = renderUnifiedDiff(out, settings().previewMaxLines ?? 5000);
  if (body.length === 0) entries.push(line([["(no textual changes)", { fg: C.dim, italic: true }]]));
  setDetail(entries.concat(body));
}

async function previewGroup(repo: Repo, group: Group, changes: Change[], token: number, title?: string): Promise<void> {
  const stats = new Map<string, string>();
  if (group !== "conflict") {
    const r = await git(repo.root, diffArgs("--numstat", ...(group === "staged" ? ["--cached", "-M"] : [])));
    for (const l of r.stdout.split("\n")) {
      const m = l.match(/^(\S+)\t(\S+)\t(.*)$/);
      if (m) stats.set(m[3].replace(/^.*\{.* => (.*)\}.*$/, "$1").replace(/^.* => /, ""), `${m[1]}\t${m[2]}`);
    }
  }
  if (token !== state.previewToken) return;
  const entries: TextPropertyEntry[] = [
    line([
      [title ?? GROUP_TITLE[group], { bold: true }],
      [`  ${repo.label} · ${title ? GROUP_TITLE[group] + " · " : ""}${changes.length} file(s)`, { fg: C.dim }],
    ]),
    line([[""]]),
  ];
  for (const c of changes) {
    const st = stats.get(c.path);
    let add = "", del = "";
    if (st) [add, del] = st.split("\t");
    entries.push(line([
      [c.code + " ", { fg: codeColor(c.code), bold: true }],
      [(add ? `+${add}` : "").padStart(6), { fg: C.added }],
      [(del ? ` -${del}` : "").padEnd(7), { fg: C.deleted }],
      ["  " + c.path],
      ...(c.origPath ? [[` ← ${c.origPath}`, { fg: C.dim }] as Part] : []),
    ]));
  }
  entries.push(line([[""]]));
  const hint = group === "staged" ? "u: unstage all · c: commit" : group === "conflict" ? "s: mark resolved" : "s: stage all · x: discard all";
  entries.push(line([[hint, { fg: C.dim, italic: true }]]));
  setDetail(entries);
}

async function previewRepo(repo: Repo, token: number): Promise<void> {
  const log = await git(repo.root, [
    "--no-optional-locks", "log", "-n", "25", "--date=relative",
    "--format=%h%x09%ad%x09%an%x09%D%x09%s",
  ]);
  const stash = await git(repo.root, ["--no-optional-locks", "stash", "list", "--format=%gd%x09%s"]);
  if (token !== state.previewToken) return;
  const branch = repo.branch === "(detached)" ? `detached at ${repo.oid.slice(0, 7)}` : repo.branch || "?";
  const entries: TextPropertyEntry[] = [
    line([[repo.label, { bold: true }], [`  ${repo.root}`, { fg: C.dim }]]),
    line([
      ["⎇ " + branch, { fg: C.branch, bold: true }],
      [repo.upstream ? `  → ${repo.upstream}` : "  (no upstream)", { fg: C.dim }],
      [repo.ahead ? `  ↑${repo.ahead} to push` : "", { fg: C.added }],
      [repo.behind ? `  ↓${repo.behind} to pull` : "", { fg: C.deleted }],
    ]),
  ];
  const counts = GROUP_ORDER.map((g) => [g, repo.changes.filter((c) => c.group === g).length] as const)
    .filter(([, n]) => n > 0)
    .map(([g, n]) => `${GROUP_TITLE[g]}: ${n}`);
  entries.push(line([[counts.length ? counts.join(" · ") : "Working tree clean", { fg: C.dim }]]));
  if (repo.error) entries.push(line([[repo.error, { fg: C.deleted }]]));
  entries.push(line([[""]]));
  entries.push(line([["Recent commits", { fg: C.header, bold: true }]]));
  if (log.exit_code !== 0) {
    entries.push(line([["(no commits yet)", { fg: C.dim, italic: true }]]));
  } else {
    for (const l of log.stdout.split("\n")) {
      if (!l) continue;
      const [hash, date, author, refs, subject] = l.split("\t");
      entries.push(line([
        [hash + " ", { fg: C.hunk }],
        [(refs ? `(${refs}) ` : ""), { fg: C.branch, bold: true }],
        [subject ?? ""],
        [`  ${author} · ${date}`, { fg: C.dim }],
      ]));
    }
  }
  const stashes = stash.stdout.split("\n").filter((l) => l);
  if (stashes.length) {
    entries.push(line([[""]]));
    entries.push(line([["Stashes", { fg: C.header, bold: true }]]));
    for (const s of stashes) {
      const [ref, msg] = s.split("\t");
      entries.push(line([[ref + " ", { fg: C.hunk }], [msg ?? ""]]));
    }
  }
  entries.push(line([[""]]));
  entries.push(line([["s: stage all · u: unstage all · c: commit · p: pull · P: push · f: fetch", { fg: C.dim, italic: true }]]));
  setDetail(entries);
}

async function updatePreview(immediate = false): Promise<void> {
  const token = ++state.previewToken;
  state.previewDiff = null;
  if (!immediate) {
    await editor.delay(120);
    if (token !== state.previewToken) return;
  }
  const row = state.rows[state.selectedIndex];
  if (!row) {
    setDetail([line([["Nothing selected", { fg: C.dim, italic: true }]])]);
    return;
  }
  try {
    if (row.kind === "file") await previewFile(row.repo, row.change, token);
    else if (row.kind === "group") await previewGroup(row.repo, row.group, row.changes, token);
    else if (row.kind === "dir") await previewGroup(row.repo, row.group, row.changes, token, row.path + "/");
    else if (row.kind === "repo") await previewRepo(row.repo, token);
    else setDetail([line([[row.text, { fg: C.dim, italic: true }]])]);
  } catch (e) {
    setDetail([line([[`Preview failed: ${e}`, { fg: C.deleted }]])]);
  }
}

// =============================================================================
// Refresh
// =============================================================================

function signature(): string {
  return JSON.stringify(state.repos);
}

async function refresh(rediscover = false, force = false): Promise<void> {
  if (!state.open) return;
  if (state.refreshing) {
    state.refreshPending = true;
    return;
  }
  state.refreshing = true;
  try {
    if (rediscover || state.roots.length === 0) state.roots = await discoverRepos(state.workspace);
    state.repos = await Promise.all(state.roots.map(loadRepo));
    if (!state.open) return;
    const sig = signature();
    if (force || sig !== state.signature) {
      state.signature = sig;
      renderTree();
      void updatePreview(true);
    }
  } finally {
    state.refreshing = false;
    if (state.refreshPending) {
      state.refreshPending = false;
      void refresh(false, true);
    }
  }
}

/** Whether the Source Control tab holds the focus. Group panels report no
 *  splits of their own, so the active buffer is the reliable signal. */
function viewFocused(): boolean {
  const active = editor.getActiveBufferId();
  return active === state.treeBufferId || active === state.detailBufferId || active === state.toolbarBufferId;
}

function scm_tick(): void {
  if (!state.open || state.busy) return;
  state.ticks++;
  // Full rate while the tab has the focus; every 5th tick otherwise (it may
  // still be on screen in another split). Saves refresh straight away.
  if (!viewFocused() && state.ticks % 5 !== 0) return;
  // Re-scan for repositories now and then (cheap: directories only).
  void refresh(state.ticks % 10 === 0);
}
registerHandler("scm_tick", scm_tick);

function startTimer(): void {
  stopTimer();
  const ms = settings().autoRefreshMs ?? 3000;
  if (ms > 0) state.timer = editor.setInterval(ms, "scm_tick");
}

function stopTimer(): void {
  if (state.timer !== null) {
    editor.clearInterval(state.timer);
    state.timer = null;
  }
}

// =============================================================================
// Selection helpers
// =============================================================================

function selectedRow(): Row | null {
  return state.rows[state.selectedIndex] ?? null;
}

/** Changes the action applies to, given the selected row. */
function targetChanges(row: Row, groups: Group[]): Change[] {
  switch (row.kind) {
    case "file": return groups.includes(row.change.group) ? [row.change] : [];
    case "group":
    case "dir": return groups.includes(row.group) ? row.changes : [];
    case "repo": return row.repo.changes.filter((c) => groups.includes(c.group));
    default: return [];
  }
}

function rowRepo(row: Row | null): Repo | null {
  if (!row) return null;
  return row.repo;
}

async function runGitOp(repo: Repo, args: string[], okMsg: string): Promise<boolean> {
  const r = await git(repo.root, args);
  if (r.exit_code !== 0) {
    editor.setStatus(`git ${args[0]} failed: ${firstLine(r.stderr) || firstLine(r.stdout)}`);
    return false;
  }
  editor.setStatus(okMsg);
  return true;
}

function plural(n: number, what: string): string {
  return `${n} ${what}${n === 1 ? "" : "s"}`;
}

async function confirm(question: string): Promise<boolean> {
  const answer = await editor.prompt(`${question} [y/N]: `, "");
  return answer !== null && /^(y|yes|s|si|sí)$/i.test(answer.trim());
}

// =============================================================================
// Hunk operations (preview pane)
// =============================================================================

/** Split a single-file patch into its header and hunks. */
function splitPatch(repo: Repo, change: Change, out: string): PreviewDiff | null {
  const lines = out.split("\n");
  if (lines.length && lines[lines.length - 1] === "") lines.pop();
  const header: string[] = [];
  const hunks: string[][] = [];
  for (const l of lines) {
    if (l.startsWith("diff --git") && (header.length || hunks.length)) break; // only the first file
    if (l.startsWith("@@")) hunks.push([l]);
    else if (hunks.length) hunks[hunks.length - 1].push(l);
    else header.push(l);
  }
  if (!hunks.length) return null;
  return {
    repo,
    change,
    header: header.join("\n") + "\n",
    hunks: hunks.map((h) => h.join("\n") + "\n"),
  };
}

function detailFocused(): boolean {
  return state.detailBufferId !== null && editor.getActiveBufferId() === state.detailBufferId;
}

function cursorProps(): { hunk?: number; newLine?: number } {
  if (state.detailBufferId === null) return {};
  const all = editor.getTextPropertiesAtCursor(state.detailBufferId) as unknown as Record<string, unknown>[];
  for (const p of all ?? []) {
    if (typeof p?.hunk === "number") return p as { hunk?: number; newLine?: number };
  }
  return {};
}

/**
 * Stage / unstage / discard the hunk under the preview cursor. Returns false
 * when the preview is not focused on a hunk, so the caller falls back to the
 * whole-file action.
 */
async function hunkOp(op: "stage" | "unstage" | "discard"): Promise<boolean> {
  if (!detailFocused()) return false;
  const pd = state.previewDiff;
  const { hunk } = cursorProps();
  if (!pd || hunk === undefined || !pd.hunks[hunk]) {
    editor.setStatus("Put the cursor on a hunk (Tab back to the list for whole files)");
    return true;
  }
  const c = pd.change;
  const allowed =
    !c.untracked && c.group !== "conflict" && !c.origPath &&
    ((op === "unstage") === (c.group === "staged"));
  if (!allowed) {
    editor.setStatus(
      c.untracked || c.origPath || c.group === "conflict"
        ? "Hunk operations need a modified tracked file; use the list for this one"
        : op === "unstage" ? "This hunk is not staged" : "This hunk is staged: unstage it first (u)",
    );
    return true;
  }
  if (op === "discard") {
    state.busy = true;
    const ok = await confirm(`Discard this hunk of ${c.path}? Cannot be undone`);
    state.busy = false;
    if (!ok) {
      editor.setStatus("Discard cancelled");
      return true;
    }
  }
  const patchPath = editor.pathJoin(scratchDir(), "hunk.patch");
  if (!writeTemp(patchPath, pd.header + pd.hunks[hunk])) return true;
  const args =
    op === "stage" ? ["apply", "--cached", "--recount", patchPath]
    : op === "unstage" ? ["apply", "--cached", "--reverse", "--recount", patchPath]
    : ["apply", "--reverse", "--recount", patchPath];
  const verb = op === "stage" ? "Staged" : op === "unstage" ? "Unstaged" : "Discarded";
  await runGitOp(pd.repo, args, `${verb} hunk ${hunk + 1}/${pd.hunks.length} of ${c.path}`);
  await refresh(false, true);
  return true;
}

/** Enter in the preview: open the file at the line under the cursor. */
function scm_detail_enter(): void {
  if (!detailFocused()) return;
  const pd = state.previewDiff;
  const row = selectedRow();
  const change = pd?.change ?? (row?.kind === "file" ? row.change : null);
  const repo = pd?.repo ?? row?.repo;
  if (!change || !repo || change.code === "D") return;
  const { newLine } = cursorProps();
  editor.openFile(editor.pathJoin(repo.root, change.path), newLine && newLine > 0 ? newLine : 1, 1);
}
registerHandler("scm_detail_enter", scm_detail_enter);

/** Indices of rows not hidden inside a collapsed repo or group. */
function visibleIndices(): number[] {
  const out: number[] = [];
  state.rows.forEach((r, i) => {
    if (!r.parents.some((k) => state.collapsed.has(k))) out.push(i);
  });
  return out;
}

function selectIndex(idx: number): void {
  const row = state.rows[idx];
  if (!row) return;
  state.selectedIndex = idx;
  state.selectedKey = row.key;
  editor.widgetMutate(TREE_PANEL, { kind: "setSelectedIndex", widgetKey: TREE_KEY, index: idx } as WidgetMutation);
  renderTree();
  void updatePreview();
}

function moveBy(delta: number): void {
  const vis = visibleIndices();
  if (!vis.length) return;
  let pos = vis.indexOf(state.selectedIndex);
  if (pos < 0) pos = 0;
  selectIndex(vis[Math.max(0, Math.min(vis.length - 1, pos + delta))]);
}

function jumpRepo(dir: 1 | -1): void {
  const vis = visibleIndices();
  const repos = vis.filter((i) => state.rows[i].kind === "repo");
  if (!repos.length) return;
  const next = dir > 0
    ? repos.find((i) => i > state.selectedIndex) ?? repos[repos.length - 1]
    : [...repos].reverse().find((i) => i < state.selectedIndex) ?? repos[0];
  selectIndex(next);
}

registerHandler("scm_first", () => { if (!detailFocused()) moveBy(-1e9); else editor.executeAction("smart_home"); });
registerHandler("scm_last", () => { if (!detailFocused()) moveBy(1e9); else editor.executeAction("move_line_end"); });
registerHandler("scm_down", () => { if (!detailFocused()) moveBy(1); else editor.executeAction("move_down"); });
registerHandler("scm_up", () => { if (!detailFocused()) moveBy(-1); else editor.executeAction("move_up"); });
registerHandler("scm_next_repo", () => jumpRepo(1));
registerHandler("scm_prev_repo", () => jumpRepo(-1));

function scm_toggle_tree(): void {
  state.treeView = !treeViewOn();
  renderTree();
  void updatePreview(true);
  renderToolbar();
  editor.setStatus(state.treeView ? "Files shown as a folder tree (t: flat list)" : "Files shown as a flat list (t: folder tree)");
}
registerHandler("scm_toggle_tree", scm_toggle_tree);

function scm_toggle_clean(): void {
  const showing = state.hideClean !== null ? !state.hideClean : settings().showCleanRepos !== false;
  state.hideClean = showing;
  renderTree();
  void updatePreview(true);
  editor.setStatus(showing ? "Hiding clean repositories (h to show)" : "Showing all repositories");
}
registerHandler("scm_toggle_clean", scm_toggle_clean);

// =============================================================================
// Actions
// =============================================================================

async function scm_stage(): Promise<void> {
  if (await hunkOp("stage")) return;
  const row = selectedRow();
  const repo = rowRepo(row);
  if (!row || !repo) return;
  const changes = targetChanges(row, ["unstaged", "conflict"]);
  if (changes.length === 0) {
    editor.setStatus("Nothing to stage here");
    return;
  }
  const all = row.kind === "repo" || (row.kind === "group" && row.group === "unstaged");
  const args = all ? ["add", "-A"] : ["add", "-A", "--", ...changes.map((c) => c.path)];
  await runGitOp(repo, args, `Staged ${plural(changes.length, "file")} in ${repo.label}`);
  await refresh(false, true);
}
registerHandler("scm_stage", scm_stage);

async function scm_unstage(): Promise<void> {
  if (await hunkOp("unstage")) return;
  const row = selectedRow();
  const repo = rowRepo(row);
  if (!row || !repo) return;
  const changes = targetChanges(row, ["staged"]);
  if (changes.length === 0) {
    editor.setStatus("Nothing staged here");
    return;
  }
  const paths: string[] = [];
  for (const c of changes) {
    paths.push(c.path);
    if (c.origPath) paths.push(c.origPath);
  }
  const unborn = repo.oid === "(initial)";
  const args = unborn ? ["rm", "-r", "-q", "--cached", "--", ...paths] : ["restore", "--staged", "--", ...paths];
  await runGitOp(repo, args, `Unstaged ${plural(changes.length, "file")} in ${repo.label}`);
  await refresh(false, true);
}
registerHandler("scm_unstage", scm_unstage);

async function scm_discard(): Promise<void> {
  if (await hunkOp("discard")) return;
  const row = selectedRow();
  const repo = rowRepo(row);
  if (!row || !repo) return;
  const changes = targetChanges(row, ["unstaged"]);
  if (changes.length === 0) {
    editor.setStatus(row.kind === "file" && row.change.group === "staged"
      ? "Staged change: unstage it first (u), then discard"
      : "Nothing to discard here");
    return;
  }
  const untracked = changes.filter((c) => c.untracked);
  const tracked = changes.filter((c) => !c.untracked);
  const desc = changes.length === 1 ? changes[0].path : plural(changes.length, "file");
  const extra = untracked.length ? ` (${untracked.length} untracked will be DELETED)` : "";
  state.busy = true;
  try {
    if (!(await confirm(`Discard changes in ${desc}${extra}? Cannot be undone`))) {
      editor.setStatus("Discard cancelled");
      return;
    }
    if (tracked.length) {
      if (!(await runGitOp(repo, ["restore", "--worktree", "--", ...tracked.map((c) => c.path)], ""))) return;
    }
    if (untracked.length) {
      if (!(await runGitOp(repo, ["clean", "-f", "-q", "--", ...untracked.map((c) => c.path)], ""))) return;
    }
    editor.setStatus(`Discarded ${plural(changes.length, "file")} in ${repo.label}`);
  } finally {
    state.busy = false;
  }
  await refresh(false, true);
}
registerHandler("scm_discard", scm_discard);

// =============================================================================
// Commit message editor (with GitHub Copilot)
// =============================================================================
//
// `c` opens the repository's COMMIT_EDITMSG as an ordinary buffer — git's own
// commit-message file, so it gets the git-commit highlighting — with the
// staged files listed in comments, as `git commit` does. Ctrl+S (or
// Ctrl+Enter) commits what is written, Esc cancels. When Copilot is
// available it writes a first draft from the staged diff; Alt+G asks again.
//
// Copilot is reached through its own language server (the one the
// fresh-copilot plugin uses, and its sign-in): a short-lived Node helper
// starts it, asks `git/commitGenerate` — the request VS Code's sparkle button
// makes — and exits. Fresh's process API has no stdin, hence the helper.

// The helper is plain JavaScript kept free of backticks and `${`, so it can
// live in this raw template literal unchanged.
const COPILOT_HELPER = String.raw`
import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import { pathToFileURL } from "node:url";
import { basename } from "node:path";

const req = JSON.parse(readFileSync(process.argv[2], "utf8"));
let child = null;
const done = (out) => {
  process.stdout.write(JSON.stringify(out) + "\n");
  try { if (child) child.kill(); } catch {}
  process.exit(0);
};
const timer = setTimeout(() => done({ ok: false, error: "Copilot did not answer within 90 s" }), 90000);
child = spawn(req.serverCommand || "copilot-language-server", req.serverArgs || ["--stdio"], {
  cwd: req.cwd,
  stdio: ["pipe", "pipe", "pipe"],
});
child.on("error", (e) => done({ ok: false, error: "cannot start " + req.serverCommand + ": " + e.message }));
child.on("exit", (code) => done({ ok: false, error: "the Copilot language server exited (" + code + ")" }));
child.stderr.on("data", () => {});

let nextId = 1;
const pending = new Map();
const send = (msg) => {
  msg.jsonrpc = "2.0";
  const body = Buffer.from(JSON.stringify(msg), "utf8");
  child.stdin.write("Content-Length: " + body.length + "\r\n\r\n");
  child.stdin.write(body);
};
const request = (method, params) => new Promise((resolve, reject) => {
  const id = nextId++;
  pending.set(id, { resolve, reject });
  send({ id, method, params });
});
const folder = { uri: pathToFileURL(req.cwd).href, name: basename(req.cwd) || "workspace" };
let buf = Buffer.alloc(0);
child.stdout.on("data", (chunk) => {
  buf = Buffer.concat([buf, chunk]);
  for (;;) {
    const sep = buf.indexOf("\r\n\r\n");
    if (sep < 0) return;
    const m = /Content-Length: *(\d+)/i.exec(buf.subarray(0, sep).toString("ascii"));
    const len = m ? parseInt(m[1], 10) : 0;
    if (buf.length < sep + 4 + len) return;
    const msg = JSON.parse(buf.subarray(sep + 4, sep + 4 + len).toString("utf8"));
    buf = buf.subarray(sep + 4 + len);
    if (msg.id !== undefined && msg.method) {
      let result = null;
      if (msg.method === "workspace/configuration") result = ((msg.params && msg.params.items) || []).map(() => ({}));
      else if (msg.method === "workspace/workspaceFolders") result = [folder];
      send({ id: msg.id, result });
    } else if (msg.id !== undefined && pending.has(msg.id)) {
      const p = pending.get(msg.id);
      pending.delete(msg.id);
      if (msg.error) p.reject(new Error(msg.error.message || JSON.stringify(msg.error)));
      else p.resolve(msg.result);
    }
  }
});
try {
  await request("initialize", {
    processId: process.pid,
    rootUri: folder.uri,
    workspaceFolders: [folder],
    capabilities: { workspace: { configuration: true, workspaceFolders: true } },
    initializationOptions: {
      editorInfo: { name: "Fresh", version: "0.5" },
      editorPluginInfo: { name: "fresh-source-control", version: "0.1.0" },
    },
  });
  send({ method: "initialized", params: {} });
  const status = await request("checkStatus", {});
  if (status && status.status && !/^(OK|AlreadySignedIn|MaybeOk)$/i.test(status.status)) {
    done({ ok: false, error: "Copilot is not signed in (" + status.status + "): run Copilot: Sign In" });
  }
  const result = await request("git/commitGenerate", {
    changes: req.changes,
    userCommits: req.userCommits || [],
    recentCommits: req.recentCommits || [],
    workspaceFolder: folder.uri,
    userLanguage: req.userLanguage,
  });
  clearTimeout(timer);
  done(result && result.commitMessage
    ? { ok: true, commitMessage: result.commitMessage }
    : { ok: false, error: "Copilot returned no message" });
} catch (e) {
  clearTimeout(timer);
  done({ ok: false, error: e.message });
}
`;

interface CopilotCommands {
  node: string;
  server: string;
  args: string[];
}

/** Reuse the fresh-copilot plugin's settings when they are there. */
function copilotCommands(): CopilotCommands {
  const cfg = editor.getConfig() as {
    plugins?: Record<string, { settings?: { nodeCommand?: string; serverCommand?: string; serverArgs?: string[] } }>;
  } | null;
  const s = cfg?.plugins?.["fresh-copilot"]?.settings ?? {};
  return {
    node: s.nodeCommand || "node",
    server: s.serverCommand || "copilot-language-server",
    args: Array.isArray(s.serverArgs) && s.serverArgs.length ? s.serverArgs : ["--stdio"],
  };
}

function copilotCommitsOn(): boolean {
  return settings().copilotCommitMessages !== false;
}

/** Commit messages of `git log`, newest first. */
async function commitMessages(repo: Repo, extra: string[]): Promise<string[]> {
  const r = await git(repo.root, ["--no-optional-locks", "log", "-n", "10", "--format=%B%x00", ...extra]);
  if (r.exit_code !== 0) return [];
  return r.stdout.split("\0").map((m) => m.trim()).filter((m) => m.length > 0).map((m) => m.slice(0, 1000));
}

/** Ask Copilot for a message describing the staged changes. */
async function copilotCommitMessage(repo: Repo): Promise<{ message?: string; error?: string }> {
  const diff = await git(repo.root, diffArgs("--cached", "-M"));
  if (diff.exit_code !== 0) return { error: firstLine(diff.stderr) || "git diff failed" };
  const changes: string[] = [];
  let total = 0;
  for (const block of diff.stdout.split(/^(?=diff --git )/m)) {
    if (!block.startsWith("diff --git ")) continue;
    // Big files and binaries add tokens, not meaning: a stub keeps the file in view.
    const text = block.length > 12000 || /^Binary files /m.test(block) ? block.split("\n")[0] + "\n(large or binary change)\n" : block;
    if (total + text.length > 80000) break;
    changes.push(text);
    total += text.length;
  }
  if (changes.length === 0) return { error: "nothing staged" };
  const email = (await git(repo.root, ["config", "user.email"])).stdout.trim();
  const cfg = editor.getConfig() as { locale?: string } | null;
  const cmds = copilotCommands();
  const dir = scratchDir();
  const helper = editor.pathJoin(dir, "copilot-commit.mjs");
  const reqPath = editor.pathJoin(dir, "copilot-commit-request.json");
  const request = {
    cwd: repo.root,
    serverCommand: cmds.server,
    serverArgs: cmds.args,
    changes,
    userCommits: email ? await commitMessages(repo, [`--author=${email}`]) : [],
    recentCommits: await commitMessages(repo, []),
    userLanguage: cfg?.locale || "en",
  };
  if (!writeTemp(helper, COPILOT_HELPER) || !writeTemp(reqPath, JSON.stringify(request))) {
    return { error: "could not write the helper script" };
  }
  const r = await editor.spawnProcess(cmds.node, [helper, reqPath], repo.root);
  const line = lastLine(r.stdout);
  try {
    const out = JSON.parse(line) as { ok: boolean; commitMessage?: string; error?: string };
    return out.ok ? { message: out.commitMessage } : { error: out.error };
  } catch {
    return { error: firstLine(r.stderr) || `${cmds.node} failed (exit ${r.exit_code})` };
  }
}

interface CommitEditor {
  repo: Repo;
  path: string;
  bufferId: number;
  /** Bumped per Copilot request, so a slow answer cannot land on a newer one. */
  token: number;
}

let commitEd: CommitEditor | null = null;

const COMMIT_HINT = "Ctrl+S commit · Esc cancel · Alt+G ask Copilot again";

/** Byte length of the message part: everything before the first comment line. */
async function messagePart(ce: CommitEditor): Promise<{ text: string; end: number }> {
  const len = editor.getBufferLength(ce.bufferId);
  const all = await editor.getBufferText(ce.bufferId, 0, len);
  const m = /^#/m.exec(all);
  const text = m ? all.slice(0, m.index) : all;
  return { text, end: utf8Len(text) };
}

/** Put Copilot's draft in the editor. `replace` is false for the automatic
 *  first draft, which must not clobber anything the user already typed. */
async function fillWithCopilot(ce: CommitEditor, replace: boolean): Promise<void> {
  const token = ++ce.token;
  editor.setStatus(`Copilot is writing a commit message for ${ce.repo.label}…`);
  const res = await copilotCommitMessage(ce.repo);
  if (commitEd !== ce || token !== ce.token) return;
  if (!res.message) {
    editor.setStatus(`Copilot: ${res.error ?? "no message"} · ${COMMIT_HINT}`);
    return;
  }
  const part = await messagePart(ce);
  if (!replace && part.text.trim() !== "") {
    editor.setStatus(`Copilot has a message ready; your text was kept (Alt+G replaces it) · ${COMMIT_HINT}`);
    return;
  }
  editor.deleteRange(ce.bufferId, 0, part.end);
  editor.insertText(ce.bufferId, 0, res.message.trim() + "\n\n");
  editor.setBufferCursor(ce.bufferId, 0);
  editor.setStatus(`Copilot wrote the message — edit it if needed · ${COMMIT_HINT}`);
}

// A file buffer has no mode of its own, so the commit keys are a window
// editor mode switched on while the commit buffer is the active one — the
// way fresh-copilot adds Tab-to-accept — and the previous mode is put back
// when it is not. Only from no mode or vi insert: another plugin's mode is
// left alone rather than fought over.
let commitModeOn = false;
let commitModePrev: string | null = null;

function syncCommitMode(): void {
  const want = commitEd !== null && editor.getActiveBufferId() === commitEd.bufferId;
  if (want && !commitModeOn) {
    // No mode reads back as undefined, not null.
    const prev = editor.getEditorMode() ?? null;
    if (prev !== null && prev !== "vi-insert") return;
    if (editor.setEditorMode("scm-commit")) {
      commitModeOn = true;
      commitModePrev = prev;
    }
  } else if (!want && commitModeOn) {
    commitModeOn = false;
    if (editor.getEditorMode() === "scm-commit") editor.setEditorMode(commitModePrev);
    commitModePrev = null;
  }
}

editor.on("buffer_activated", () => syncCommitMode());

function closeCommitEditor(): void {
  const ce = commitEd;
  commitEd = null;
  syncCommitMode();
  if (ce) editor.closeBuffer(ce.bufferId, true);
  if (state.open && state.groupId !== null) editor.focusBufferGroupPanel(state.groupId, "tree");
}

/** Stage-all offer shared by both commit commands. Returns the staged count. */
async function ensureStaged(repo: Repo): Promise<number> {
  let staged = repo.changes.filter((c) => c.group === "staged").length;
  if (staged > 0) return staged;
  if (repo.changes.length === 0) {
    editor.setStatus(`${repo.label}: nothing to commit`);
    return 0;
  }
  if (!(await confirm(`${repo.label}: nothing staged. Stage ALL ${repo.changes.length} changes and commit?`))) return 0;
  if (!(await runGitOp(repo, ["add", "-A"], ""))) return 0;
  staged = repo.changes.length;
  return staged;
}

async function scm_commit(): Promise<void> {
  const repo = rowRepo(selectedRow());
  if (!repo) return;
  state.busy = true;
  let staged = 0;
  try {
    staged = await ensureStaged(repo);
  } finally {
    state.busy = false;
  }
  if (staged === 0) return;
  const gp = await git(repo.root, ["rev-parse", "--git-path", "COMMIT_EDITMSG"]);
  if (gp.exit_code !== 0) {
    editor.setStatus(`${repo.label}: ${firstLine(gp.stderr)}`);
    return;
  }
  const rel = gp.stdout.trim();
  const path = rel.startsWith("/") ? rel : editor.pathJoin(repo.root, rel);
  const names = await git(repo.root, ["--no-optional-locks", "-c", "core.quotepath=false", "diff", "--cached", "--name-status", "-M"]);
  const files = names.stdout.split("\n").filter((l) => l).map((l) => {
    const [code, ...paths] = l.split("\t");
    const what = { M: "modified", A: "new file", D: "deleted", R: "renamed", C: "copied", T: "typechange" }[code[0]] ?? code;
    return `#\t${what}:   ${paths.join(" -> ")}`;
  });
  const template = [
    "",
    "",
    `# ${COMMIT_HINT}.`,
    "# Lines starting with '#' are ignored; an empty message aborts the commit.",
    "#",
    `# On branch ${repo.branch} (${repo.label})`,
    "# Changes to be committed:",
    ...files,
    "",
  ].join("\n");
  if (commitEd) closeCommitEditor();
  // A buffer already showing the file (a cancelled editor restored with the
  // session, say) would be reused as is, and the new template never shown.
  const stale = editor.findBufferByPath(path);
  if (stale) editor.closeBuffer(stale, true);
  if (!writeTemp(path, template)) return;
  editor.openFile(path, 1, 1);
  await editor.flush();
  const bufferId = editor.findBufferByPath(path);
  if (!bufferId) {
    editor.setStatus(`Could not open ${path}`);
    return;
  }
  const ce: CommitEditor = { repo, path, bufferId, token: 0 };
  commitEd = ce;
  syncCommitMode();
  if (copilotCommitsOn()) void fillWithCopilot(ce, false);
  else editor.setStatus(`Write the commit message for ${repo.label} · ${COMMIT_HINT}`);
}
registerHandler("scm_commit", scm_commit);

async function scm_commit_confirm(): Promise<void> {
  const ce = commitEd;
  if (!ce) return;
  const len = editor.getBufferLength(ce.bufferId);
  const text = await editor.getBufferText(ce.bufferId, 0, len);
  const message = text.split("\n").filter((l) => !l.startsWith("#")).join("\n").trim();
  if (!message) {
    editor.setStatus(`Empty message: nothing committed · ${COMMIT_HINT}`);
    return;
  }
  if (!writeTemp(ce.path, text)) return;
  editor.setStatus(`Committing to ${ce.repo.label}…`);
  const r = await git(ce.repo.root, ["commit", "--cleanup=strip", "-F", ce.path]);
  if (r.exit_code !== 0) {
    // Keep the editor open: a failed hook should not cost the message.
    editor.setStatus(`Commit failed: ${firstLine(r.stderr) || firstLine(r.stdout)}`);
    return;
  }
  closeCommitEditor();
  editor.setStatus(`${ce.repo.label}: ${firstLine(r.stdout)}`);
  await refresh(false, true);
}
registerHandler("scm_commit_confirm", scm_commit_confirm);

function scm_commit_cancel(): void {
  if (!commitEd) return;
  closeCommitEditor();
  editor.setStatus("Commit cancelled");
}
registerHandler("scm_commit_cancel", scm_commit_cancel);

function scm_commit_copilot(): void {
  if (commitEd) void fillWithCopilot(commitEd, true);
}
registerHandler("scm_commit_copilot", scm_commit_copilot);

/** `C`: the quick one-line commit, through the prompt. */
async function scm_commit_quick(): Promise<void> {
  const repo = rowRepo(selectedRow());
  if (!repo) return;
  state.busy = true;
  try {
    const staged = await ensureStaged(repo);
    if (staged === 0) return;
    const msg = await editor.prompt(`Commit ${plural(staged, "file")} to ${repo.label} (${repo.branch}) — message: `, "");
    if (msg === null || !msg.trim()) {
      editor.setStatus("Commit cancelled");
      return;
    }
    editor.setStatus(`Committing to ${repo.label}…`);
    const r = await git(repo.root, ["commit", "-m", msg.trim()]);
    editor.setStatus(r.exit_code !== 0
      ? `Commit failed: ${firstLine(r.stderr) || firstLine(r.stdout)}`
      : `${repo.label}: ${firstLine(r.stdout)}`);
  } finally {
    state.busy = false;
  }
  await refresh(false, true);
}
registerHandler("scm_commit_quick", scm_commit_quick);

// The commit buffer keeps ordinary editing; these keys are added on top.
editor.defineMode(
  "scm-commit",
  [
    ["C-s", "scm_commit_confirm"],
    ["C-Enter", "scm_commit_confirm"],
    ["Escape", "scm_commit_cancel"],
    ["M-g", "scm_commit_copilot"],
  ],
  false, // editable
  true, // allow_text_input
  true, // inherit Normal-context bindings for unbound keys
);

editor.on("buffer_closed", (data) => {
  if (commitEd && data.buffer_id === commitEd.bufferId) {
    commitEd = null;
    syncCommitMode();
  }
});

async function networkOp(verb: string, build: (repo: Repo) => string[] | null): Promise<void> {
  const repo = rowRepo(selectedRow());
  if (!repo) return;
  const args = build(repo);
  if (!args) return;
  state.busy = true;
  try {
    editor.setStatus(`${repo.label}: git ${args.join(" ")}…`);
    const r = await gitNetwork(repo.root, args);
    if (r.exit_code !== 0) {
      editor.setStatus(`${repo.label}: ${verb} failed: ${lastLine(r.stderr) || lastLine(r.stdout)}`);
    } else {
      editor.setStatus(`${repo.label}: ${verb} done. ${lastLine(r.stdout) || lastLine(r.stderr)}`);
    }
  } finally {
    state.busy = false;
  }
  await refresh(false, true);
}

async function scm_pull(): Promise<void> {
  await networkOp("pull", () => ["pull", "--ff-only"]);
}
registerHandler("scm_pull", scm_pull);

async function scm_push(): Promise<void> {
  const repo = rowRepo(selectedRow());
  if (!repo) return;
  if (repo.branch === "(detached)") {
    editor.setStatus(`${repo.label}: detached HEAD, nothing to push`);
    return;
  }
  let args = ["push"];
  if (repo.upstream === null) {
    state.busy = true;
    const ok = await confirm(`${repo.label}: '${repo.branch}' has no upstream. Push and set upstream to origin/${repo.branch}?`);
    state.busy = false;
    if (!ok) return;
    args = ["push", "-u", "origin", repo.branch];
  }
  await networkOp("push", () => args);
}
registerHandler("scm_push", scm_push);

async function scm_fetch(): Promise<void> {
  await networkOp("fetch", () => ["fetch", "--prune"]);
}
registerHandler("scm_fetch", scm_fetch);

// =============================================================================
// Branch switching
// =============================================================================

interface BranchItem {
  name: string;
  remote: boolean;
  current: boolean;
  desc: string;
}

const branchPrompt = {
  repo: null as Repo | null,
  items: [] as BranchItem[],
  shown: [] as PromptSuggestion[],
};

async function scm_branch(): Promise<void> {
  const repo = rowRepo(selectedRow());
  if (!repo) return;
  const r = await git(repo.root, [
    "for-each-ref", "--sort=-committerdate",
    "--format=%(refname)%09%(refname:short)%09%(committerdate:relative)%09%(subject)",
    "refs/heads", "refs/remotes",
  ]);
  if (r.exit_code !== 0) {
    editor.setStatus(`${repo.label}: ${firstLine(r.stderr)}`);
    return;
  }
  const items: BranchItem[] = [];
  const locals = new Set<string>();
  for (const l of r.stdout.split("\n")) {
    if (!l) continue;
    const [full, short, date, subject] = l.split("\t");
    if (full.endsWith("/HEAD")) continue;
    const remote = full.startsWith("refs/remotes/");
    if (!remote) locals.add(short);
    items.push({ name: short, remote, current: !remote && short === repo.branch, desc: `${date} · ${subject ?? ""}` });
  }
  // A remote branch that already has a local namesake is reached through the local one.
  branchPrompt.items = items.filter((it) => !it.remote || !locals.has(it.name.slice(it.name.indexOf("/") + 1)));
  branchPrompt.repo = repo;
  editor.startPrompt(`Switch branch in ${repo.label} (on ${repo.branch}): `, "scm-branch");
  setBranchSuggestions("");
}
registerHandler("scm_branch", scm_branch);

function setBranchSuggestions(input: string): void {
  const q = input.trim();
  const ql = q.toLowerCase();
  const list: PromptSuggestion[] = branchPrompt.items
    .filter((it) => !ql || it.name.toLowerCase().includes(ql))
    .slice(0, 200)
    .map((it) => ({
      id: (it.remote ? "R:" : "L:") + it.name,
      text: (it.current ? "● " : "  ") + it.name,
      description: (it.remote ? "remote · " : "") + it.desc,
      value: (it.remote ? "R:" : "L:") + it.name,
    }));
  if (q && !branchPrompt.items.some((it) => it.name === q)) {
    list.push({ id: "C:" + q, text: `+ Create branch '${q}'`, description: "from the current HEAD", value: "C:" + q });
  }
  branchPrompt.shown = list;
  editor.setPromptSuggestions(list, list.length ? 0 : null);
}

async function switchBranch(input: string, idx: number | null): Promise<void> {
  const repo = branchPrompt.repo;
  branchPrompt.repo = null;
  if (!repo) return;
  let choice = idx !== null && idx >= 0 ? branchPrompt.shown[idx]?.value : undefined;
  if (!choice) {
    const name = input.trim();
    if (!name) return;
    choice = branchPrompt.items.some((it) => it.name === name && !it.remote) ? "L:" + name : "C:" + name;
  }
  const kind = choice.slice(0, 2);
  const name = choice.slice(2);
  let args: string[];
  let target = name;
  if (kind === "L:") {
    args = ["switch", name];
  } else if (kind === "R:") {
    target = name.slice(name.indexOf("/") + 1);
    args = ["switch", "-c", target, "--track", name];
  } else {
    args = ["switch", "-c", name];
  }
  if (target === repo.branch) {
    editor.setStatus(`${repo.label}: already on ${target}`);
    return;
  }
  await runGitOp(repo, args, `${repo.label}: switched to ${target}`);
  await refresh(false, true);
}

editor.on("prompt_changed", (e) => {
  if (e.prompt_type === "scm-branch") setBranchSuggestions(e.input);
});
editor.on("prompt_confirmed", (e) => {
  if (e.prompt_type === "scm-branch") void switchBranch(e.input, e.selected_index);
});
editor.on("prompt_cancelled", (e) => {
  if (e.prompt_type === "scm-branch") branchPrompt.repo = null;
});

async function scm_refresh(): Promise<void> {
  editor.setStatus("Source Control: refreshing…");
  await refresh(true, true);
  editor.setStatus(`Source Control: ${plural(state.repos.length, "repository").replace("repositorys", "repositories")}`);
}
registerHandler("scm_refresh", scm_refresh);

function scm_open_file(): void {
  const row = selectedRow();
  if (!row || row.kind !== "file") return;
  if (row.change.code === "D") {
    editor.setStatus("File is deleted");
    return;
  }
  editor.openFile(editor.pathJoin(row.repo.root, row.change.path), null, null);
}
registerHandler("scm_open_file", scm_open_file);

function scm_tab(): void {
  if (state.groupId === null) return;
  const active = editor.getActiveBufferId();
  editor.focusBufferGroupPanel(state.groupId, active === state.detailBufferId ? "tree" : "detail");
}
registerHandler("scm_tab", scm_tab);

function scm_help(): void {
  const keys: [string, string][] = [
    ["↑/↓ j/k ←/→", "move · collapse/expand"],
    ["Home/End  ] [", "first / last row · next / previous repository"],
    ["Enter", "file: side-by-side diff · repo/group: collapse/expand"],
    ["o", "open file"],
    ["s", "stage (file, group or whole repo)"],
    ["u", "unstage"],
    ["x", "discard changes (asks first; untracked files are deleted)"],
    ["c", "commit: message editor, drafted by Copilot (Ctrl+S commit · Esc cancel · Alt+G redo)"],
    ["C", "quick one-line commit"],
    ["b", "switch / create branch in the selected repo"],
    ["+ / − buttons", "click to stage / unstage a file or a whole group"],
    ["p / P / f", "pull --ff-only / push / fetch --prune"],
    ["r", "refresh and re-scan repositories"],
    ["h", "hide / show clean repositories"],
    ["t", "files as a flat list / folder tree"],
    ["Tab → s/u/x", "in the preview: stage / unstage / discard the hunk under the cursor"],
    ["Tab → Enter", "in the preview: open the file at that line"],
    ["Tab", "switch between tree and preview"],
    ["q", "close"],
    ["", ""],
    ["In the diff tab", "n/p next/prev hunk · Enter open file at line · q close"],
  ];
  setDetail([
    line([["Source Control — keys", { fg: C.header, bold: true }]]),
    line([[""]]),
    ...keys.map(([k, d]) => line([[k.padEnd(18), { fg: C.hunk, bold: true }], [d]])),
    line([[""]]),
    line([[`Workspace: ${state.workspace}`, { fg: C.dim }]]),
    line([[`Settings: plugins.source_control.settings (scanDepth, showCleanRepos, autoRefreshMs, previewMaxLines)`, { fg: C.dim }]]),
  ]);
}
registerHandler("scm_help", scm_help);

// =============================================================================
// Side-by-side diff
// =============================================================================

interface Hunk {
  oldStart: number;
  newStart: number;
  lines: string[];
}

function parseHunks(out: string): Hunk[] {
  const hunks: Hunk[] = [];
  let cur: Hunk | null = null;
  for (const l of out.split("\n")) {
    if (l.startsWith("diff --git")) {
      cur = null;
      continue;
    }
    const m = l.match(/^@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@/);
    if (m) {
      cur = { oldStart: parseInt(m[1]), newStart: parseInt(m[2]), lines: [] };
      hunks.push(cur);
      continue;
    }
    if (cur && (l[0] === "+" || l[0] === "-" || l[0] === " ")) cur.lines.push(l);
  }
  return hunks;
}

function toCompositeHunks(hunks: Hunk[]): TsCompositeHunk[] {
  return hunks.map((h) => {
    let oldCount = 0;
    let newCount = 0;
    let ops = "";
    for (const l of h.lines) {
      ops += l[0];
      if (l[0] !== "+") oldCount++;
      if (l[0] !== "-") newCount++;
    }
    return {
      oldStart: Math.max(0, h.oldStart - 1),
      oldCount: Math.max(1, oldCount),
      newStart: Math.max(0, h.newStart - 1),
      newCount,
      ops,
    };
  });
}

function contentEntries(content: string): TextPropertyEntry[] {
  const lines = content.split("\n");
  if (lines.length > 1 && lines[lines.length - 1] === "") lines.pop();
  const text = lines.join("\n");
  return text.length > 0 ? [{ text }] : [];
}

async function gitShow(repo: Repo, spec: string): Promise<string> {
  const r = await git(repo.root, ["--no-optional-locks", "show", spec]);
  return r.exit_code === 0 ? r.stdout : "";
}

/** Per-user scratch directory for diff inputs and hunk patches. */
function scratchDir(): string {
  const user = editor.getEnv("USER") || "user";
  return editor.pathJoin(editor.getTempDir(), `fresh-source-control-${user}`);
}

/** Write a scratch file, replacing a previous one (`writeFile` refuses to). */
function writeTemp(path: string, content: string): boolean {
  const ok = editor.fileExists(path) ? editor.replaceFile(path, content) : editor.writeFile(path, content);
  if (!ok) editor.setStatus(`Source Control: could not write ${path}`);
  return ok;
}

function closeComposite(): void {
  const cs = state.composite;
  if (!cs) return;
  state.composite = null;
  try {
    editor.closeCompositeBuffer(cs.compositeId);
  } catch {}
  try {
    editor.closeBuffer(cs.oldId);
    editor.closeBuffer(cs.newId);
  } catch {}
}

async function scm_open_diff(): Promise<void> {
  const row = selectedRow();
  if (!row) return;
  if (row.kind !== "file") {
    if (row.kind === "repo" || row.kind === "group" || row.kind === "dir") toggleRow(row);
    return;
  }
  const { repo, change: c } = row;
  const abs = editor.pathJoin(repo.root, c.path);
  if (editor.fileExists(editor.authorityPath(editor.pathJoin(abs, ".git")))) {
    editor.setStatus(`${c.path} is a submodule — its commit change is in the preview`);
    return;
  }
  editor.setStatus(`Loading diff for ${c.path}…`);
  if (settings().nativeDiffView === true) await openNativeDiff(repo, c);
  else await openDiffView(repo, c);
}
registerHandler("scm_open_diff", scm_open_diff);

// =============================================================================
// Loading the two sides of a diff
// =============================================================================

interface DiffPair {
  oldText: string;
  newText: string;
  oldLabel: string;
  newLabel: string;
}

/** The two versions VS Code compares: HEAD ↔ index for a staged change,
 *  index ↔ working tree for an unstaged one. */
async function loadDiffPair(repo: Repo, c: Change): Promise<DiffPair> {
  const abs = editor.pathJoin(repo.root, c.path);
  const readDisk = () => editor.readFile(editor.authorityPath(abs)) ?? "";
  if (c.group === "staged") {
    return {
      oldText: c.code === "A" ? "" : await gitShow(repo, `HEAD:${c.origPath ?? c.path}`),
      newText: c.code === "D" ? "" : await gitShow(repo, `:${c.path}`),
      oldLabel: c.code === "A" ? "(new file)" : `HEAD  ${c.origPath ?? c.path}`,
      newLabel: c.code === "D" ? "(deleted)" : `Index (staged)  ${c.path}`,
    };
  }
  if (c.group === "conflict") {
    return {
      oldText: await gitShow(repo, `HEAD:${c.path}`),
      newText: readDisk(),
      oldLabel: `HEAD  ${c.path}`,
      newLabel: `Working tree (conflicted)  ${c.path}`,
    };
  }
  // Whether the index differs from HEAD for this file right now (the repo
  // snapshot may predate a per-change stage done from the diff view).
  const alsoStaged = !c.untracked &&
    (await git(repo.root, ["--no-optional-locks", "diff", "--cached", "--quiet", "--", c.path])).exit_code === 1;
  return {
    oldText: c.untracked ? "" : await gitShow(repo, `:${c.path}`),
    newText: c.code === "D" ? "" : readDisk(),
    oldLabel: c.untracked ? "(new file)" : alsoStaged ? `Index (staged)  ${c.path}` : `HEAD  ${c.path}`,
    newLabel: c.code === "D" ? "(deleted)" : `Working tree  ${c.path}`,
  };
}

let diffSeq = 0;

/** `git diff --no-index` over two texts, so the hunks describe exactly what
 *  is shown. `bigFile` raises git's binary cut-off for the diff view, which
 *  only ever renders a screenful. */
async function diffTexts(oldText: string, newText: string, unified: number, bigFile = "1m"): Promise<string | null> {
  const tmp = scratchDir();
  const n = diffSeq++ % 8;
  const oldPath = editor.pathJoin(tmp, `old-${n}`);
  const newPath = editor.pathJoin(tmp, `new-${n}`);
  if (!writeTemp(oldPath, oldText) || !writeTemp(newPath, newText)) return null;
  const args = diffArgs("--no-index", `--unified=${unified}`, "--", oldPath, newPath);
  args.splice(args.indexOf("diff"), 0, "-c", `core.bigFileThreshold=${bigFile}`);
  const d = await editor.spawnProcess("git", args, tmp);
  return d.stdout;
}

// =============================================================================
// Native side-by-side (Fresh's composite buffer) — `nativeDiffView: true`
// =============================================================================

async function openNativeDiff(repo: Repo, c: Change): Promise<void> {
  const abs = editor.pathJoin(repo.root, c.path);
  const { oldText, newText, oldLabel, newLabel } = await loadDiffPair(repo, c);
  const out = await diffTexts(oldText, newText, 3);
  if (out === null) return;
  if (/^Binary files /m.test(out)) {
    editor.setStatus(`${c.path}: binary file (or larger than 1 MiB), no side-by-side diff`);
    return;
  }
  const hunks = parseHunks(out);
  if (hunks.length === 0) {
    editor.setStatus(`${c.path}: no textual changes`);
    return;
  }
  closeComposite();
  // Name the sources after the file so the host picks the grammar from the extension.
  const base = editor.pathBasename(c.path);
  const source = async (name: string, text: string) =>
    (await editor.createVirtualBuffer({
      name,
      mode: "normal",
      readOnly: true,
      entries: contentEntries(text),
      showLineNumbers: true,
      editingDisabled: true,
      hiddenFromTabs: true,
    })).bufferId;
  const oldId = await source(`old ${base}`, oldText);
  const newId = await source(`new ${base}`, newText);
  const compositeId = await editor.createCompositeBuffer({
    name: `*Diff: ${base}*`,
    mode: "scm-diff",
    layout: { type: "side-by-side", ratios: [0.5, 0.5], showSeparator: true },
    sources: [
      { bufferId: oldId, label: oldLabel, editable: false, style: { gutterStyle: "diff-markers" } },
      { bufferId: newId, label: newLabel, editable: false, style: { gutterStyle: "diff-markers" } },
    ],
    hunks: toCompositeHunks(hunks),
    initialFocusHunk: 0,
  });
  state.composite = { compositeId, oldId, newId, absPath: abs };
  editor.showBuffer(compositeId);
  editor.setStatus(`${c.path}: ${plural(hunks.length, "hunk")} · n/p hunks · Enter open · q close`);
}

async function scm_diff_open_file(): Promise<void> {
  const cs = state.composite;
  if (!cs) return;
  const info = await editor.getCompositeCursorInfo();
  const newLine = info?.lines?.[1];
  const ln = typeof newLine === "number" ? newLine + 1 : 1;
  if (!editor.fileExists(editor.authorityPath(cs.absPath))) {
    editor.setStatus("File does not exist on disk");
    return;
  }
  editor.openFile(cs.absPath, ln, 1);
}
registerHandler("scm_diff_open_file", scm_diff_open_file);

// The composite keymap already provides q (close) and n/p (hunks).
editor.defineMode("scm-diff", [
  ["Enter", "scm_diff_open_file"],
  ["o", "scm_diff_open_file"],
], true);

// =============================================================================
// Diff view — compact side-by-side
// =============================================================================
//
// Two panels (old | new) whose rows the plugin lays out itself: only the
// changed regions plus a few lines of context are shown, the rest folds into
// "⋯ N unchanged lines" rows, a modified line sits next to its new version
// with the changed words highlighted, and the side that has no line gets a
// hatched filler. The panels are not scrollable — the plugin renders the
// visible window into both, so they can never drift apart — and the code is
// highlighted by the host through syntax regions that skip the gutter.

interface DiffBlock {
  /** 0-based first old line (the insertion point when `oldCount` is 0). */
  oldStart: number;
  oldCount: number;
  newStart: number;
  newCount: number;
  /** The zero-context hunk as git printed it, for `git apply --unidiff-zero`. */
  patch: string;
}

interface DiffRow {
  o: number | null;
  n: number | null;
  chg: boolean;
  /** Index into `blocks`, -1 for unchanged rows. */
  block: number;
}

type DiffItem = { kind: "row"; row: number } | { kind: "fold"; from: number; to: number };

interface DiffView {
  groupId: number;
  bufs: { hdr: number; old: number; new: number };
  repo: Repo;
  change: Change;
  absPath: string;
  oldLabel: string;
  newLabel: string;
  oldLines: string[];
  newLines: string[];
  rows: DiffRow[];
  blocks: DiffBlock[];
  added: number;
  removed: number;
  full: boolean;
  /** Fold start rows the reader expanded. */
  expanded: Set<number>;
  items: DiffItem[];
  /** One entry per screen row: the item and which wrapped part of it. */
  visual: { item: number; part: number }[];
  leftWidth: number;
  rightWidth: number;
  height: number;
  digits: number;
  codeWidth: number;
  top: number;
  cursor: number;
  inline: Map<number, [number, number][][]>;
}

let dv: DiffView | null = null;

const DIFF_LAYOUT = JSON.stringify({
  type: "split",
  direction: "v",
  ratio: 0.05,
  first: { type: "fixed", id: "hdr", height: 1 },
  second: {
    type: "split",
    direction: "h",
    ratio: 0.5,
    first: { type: "scrollable", id: "old", scrollable: false },
    second: { type: "scrollable", id: "new", scrollable: false },
  },
});

function utf8Len(s: string): number {
  let n = 0;
  for (let i = 0; i < s.length; i++) {
    const c = s.charCodeAt(i);
    if (c < 0x80) n += 1;
    else if (c < 0x800) n += 2;
    else if (c >= 0xd800 && c <= 0xdbff) { n += 4; i++; }
    else n += 3;
  }
  return n;
}

function tabSize(): number {
  const cfg = editor.getConfig() as { editor?: { tab_size?: number } } | null;
  const t = cfg?.editor?.tab_size;
  return typeof t === "number" && t > 0 ? t : 4;
}

/** Lines as drawn: tabs expanded, CR dropped, no phantom last line. */
function displayLines(text: string): string[] {
  const ts = tabSize();
  const lines = text.split("\n");
  if (lines.length && lines[lines.length - 1] === "") lines.pop();
  return lines.map((l) => {
    if (l.endsWith("\r")) l = l.slice(0, -1);
    if (!l.includes("\t")) return l;
    let out = "";
    for (const ch of l) {
      if (ch === "\t") out += " ".repeat(ts - (out.length % ts));
      else out += ch;
    }
    return out;
  });
}

function parseBlocks(out: string): DiffBlock[] {
  const blocks: DiffBlock[] = [];
  let cur: DiffBlock | null = null;
  for (const l of out.split("\n")) {
    const m = l.match(/^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@/);
    if (m) {
      const a = parseInt(m[1]);
      const b = m[2] === undefined ? 1 : parseInt(m[2]);
      const c = parseInt(m[3]);
      const d = m[4] === undefined ? 1 : parseInt(m[4]);
      cur = { oldStart: b > 0 ? a - 1 : a, oldCount: b, newStart: d > 0 ? c - 1 : c, newCount: d, patch: l + "\n" };
      blocks.push(cur);
    } else if (cur && (l[0] === "+" || l[0] === "-" || l[0] === "\\")) {
      cur.patch += l + "\n";
    } else if (l.startsWith("diff --git")) {
      cur = null;
    }
  }
  return blocks;
}

/** Every line of both files on one row each: unchanged lines paired, and
 *  inside a change block old and new lines paired positionally, so a
 *  modified line sits next to its replacement. */
function alignRows(blocks: DiffBlock[], oldN: number, newN: number): DiffRow[] {
  const rows: DiffRow[] = [];
  let oi = 0;
  let ni = 0;
  blocks.forEach((b, bi) => {
    while (oi < b.oldStart && ni < b.newStart) rows.push({ o: oi++, n: ni++, chg: false, block: -1 });
    while (oi < b.oldStart) rows.push({ o: oi++, n: null, chg: true, block: -1 });
    while (ni < b.newStart) rows.push({ o: null, n: ni++, chg: true, block: -1 });
    const m = Math.max(b.oldCount, b.newCount);
    for (let k = 0; k < m; k++) {
      rows.push({
        o: k < b.oldCount ? b.oldStart + k : null,
        n: k < b.newCount ? b.newStart + k : null,
        chg: true,
        block: bi,
      });
    }
    oi = b.oldStart + b.oldCount;
    ni = b.newStart + b.newCount;
  });
  while (oi < oldN && ni < newN) rows.push({ o: oi++, n: ni++, chg: false, block: -1 });
  while (oi < oldN) rows.push({ o: oi++, n: null, chg: true, block: -1 });
  while (ni < newN) rows.push({ o: null, n: ni++, chg: true, block: -1 });
  return rows;
}

/** Changed character ranges of a modified line, by a token-level LCS. */
function inlineDiff(a: string, b: string): [number, number][][] {
  const tok = (s: string) => s.match(/\w+|\s+|[^\w\s]/g) ?? [];
  const ta = tok(a);
  const tb = tok(b);
  const n = ta.length;
  const m = tb.length;
  if (n * m > 160000) return [[[0, a.length]], [[0, b.length]]];
  const W = m + 1;
  const dp = new Uint16Array((n + 1) * W);
  for (let i = n - 1; i >= 0; i--) {
    for (let j = m - 1; j >= 0; j--) {
      dp[i * W + j] = ta[i] === tb[j] ? dp[(i + 1) * W + j + 1] + 1 : Math.max(dp[(i + 1) * W + j], dp[i * W + j + 1]);
    }
  }
  const ra: [number, number][] = [];
  const rb: [number, number][] = [];
  const push = (r: [number, number][], s: number, e: number) => {
    const last = r[r.length - 1];
    if (last && last[1] === s) last[1] = e;
    else r.push([s, e]);
  };
  let i = 0, j = 0, pa = 0, pb = 0;
  while (i < n && j < m) {
    if (ta[i] === tb[j]) {
      pa += ta[i++].length;
      pb += tb[j++].length;
    } else if (dp[(i + 1) * W + j] >= dp[i * W + j + 1]) {
      push(ra, pa, pa + ta[i].length);
      pa += ta[i++].length;
    } else {
      push(rb, pb, pb + tb[j].length);
      pb += tb[j++].length;
    }
  }
  while (i < n) { push(ra, pa, pa + ta[i].length); pa += ta[i++].length; }
  while (j < m) { push(rb, pb, pb + tb[j].length); pb += tb[j++].length; }
  return [ra, rb];
}

function inlineFor(v: DiffView, row: number): [number, number][][] {
  let r = v.inline.get(row);
  if (!r) {
    const dr = v.rows[row];
    r = dr.o !== null && dr.n !== null ? inlineDiff(v.oldLines[dr.o], v.newLines[dr.n]) : [[], []];
    v.inline.set(row, r);
  }
  return r;
}

async function computeDiff(v: DiffView, pair: DiffPair): Promise<string | null> {
  if (pair.oldText.includes("\0") || pair.newText.includes("\0")) return "binary file, no text diff";
  if (pair.oldText.length + pair.newText.length > 16 * 1024 * 1024) return "file too large for the diff view";
  const out = await diffTexts(pair.oldText, pair.newText, 0, "16m");
  if (out === null) return "could not run git diff";
  if (/^Binary files /m.test(out)) return "binary file, no text diff";
  v.oldLines = displayLines(pair.oldText);
  v.newLines = displayLines(pair.newText);
  v.blocks = parseBlocks(out);
  v.rows = alignRows(v.blocks, v.oldLines.length, v.newLines.length);
  v.added = v.rows.filter((r) => r.chg && r.n !== null).length;
  v.removed = v.rows.filter((r) => r.chg && r.o !== null).length;
  v.oldLabel = pair.oldLabel;
  v.newLabel = pair.newLabel;
  v.inline.clear();
  v.expanded.clear();
  return null;
}

function diffContext(): number {
  const n = settings().diffContextLines;
  return typeof n === "number" && n >= 0 ? n : 3;
}

function buildItems(v: DiffView): DiffItem[] {
  const n = v.rows.length;
  const items: DiffItem[] = [];
  if (v.full || !v.rows.some((r) => r.chg)) {
    for (let i = 0; i < n; i++) items.push({ kind: "row", row: i });
    return items;
  }
  const ctx = diffContext();
  const show = new Uint8Array(n);
  v.rows.forEach((r, i) => {
    if (!r.chg) return;
    for (let j = Math.max(0, i - ctx); j <= Math.min(n - 1, i + ctx); j++) show[j] = 1;
  });
  let i = 0;
  while (i < n) {
    if (show[i]) {
      items.push({ kind: "row", row: i++ });
      continue;
    }
    let j = i;
    while (j < n && !show[j]) j++;
    if (j - i < 3 || v.expanded.has(i)) {
      for (let k = i; k < j; k++) items.push({ kind: "row", row: k });
    } else {
      items.push({ kind: "fold", from: i, to: j });
    }
    i = j;
  }
  return items;
}

/** The aligned row under the cursor (a fold reports its first row). */
function cursorRow(v: DiffView): number {
  const vr = v.visual[v.cursor];
  if (!vr) return 0;
  const it = v.items[vr.item];
  return it.kind === "row" ? it.row : it.from;
}

function visualForRow(v: DiffView, row: number): number {
  for (let i = 0; i < v.visual.length; i++) {
    const it = v.items[v.visual[i].item];
    if (it.kind === "row" ? it.row >= row : it.to > row) return i;
  }
  return Math.max(0, v.visual.length - 1);
}

function prefixWidth(v: DiffView): number {
  return v.digits + 3;
}

/** Rebuild items and screen rows, keeping the cursor on the same line. */
function relayout(v: DiffView, keepRow: number | null): void {
  v.digits = Math.max(3, String(Math.max(v.oldLines.length, v.newLines.length)).length);
  v.codeWidth = Math.max(10, Math.min(v.leftWidth, v.rightWidth) - prefixWidth(v) - 1);
  v.items = buildItems(v);
  v.visual = [];
  const W = v.codeWidth;
  v.items.forEach((it, ii) => {
    let parts = 1;
    if (it.kind === "row") {
      const r = v.rows[it.row];
      const lo = r.o !== null ? v.oldLines[r.o].length : 0;
      const ln = r.n !== null ? v.newLines[r.n].length : 0;
      parts = Math.max(1, Math.ceil(lo / W), Math.ceil(ln / W));
    }
    for (let p = 0; p < parts; p++) v.visual.push({ item: ii, part: p });
  });
  if (keepRow !== null) v.cursor = visualForRow(v, keepRow);
  clampView(v);
}

function clampView(v: DiffView): void {
  const last = Math.max(0, v.visual.length - 1);
  v.cursor = Math.max(0, Math.min(last, v.cursor));
  if (v.cursor < v.top) v.top = v.cursor;
  if (v.cursor >= v.top + v.height) v.top = v.cursor - v.height + 1;
  v.top = Math.max(0, Math.min(v.top, Math.max(0, v.visual.length - v.height)));
}

// ASCII on purpose: box-drawing diagonals are measured as wide by the host,
// which shifts everything after them on the row.
const HATCH = "/";

function renderDiffHeader(v: DiffView): void {
  const fit = (s: string, w: number) => (s.length > w ? s.slice(0, Math.max(0, w - 1)) + "…" : s.padEnd(w));
  const changes = v.blocks.length;
  const right = ` ${v.newLabel}`;
  const stats = `   +${v.added} -${v.removed} · ${plural(changes, "change")}${v.full ? " · full file" : ""}`;
  editor.setPanelContent(v.groupId, "hdr", [
    { text: fit(` ${v.oldLabel}`, v.leftWidth), style: { fg: C.deleted, bold: true } },
    { text: "│", style: { fg: C.dim } },
    { text: right, style: { fg: C.added, bold: true } },
    { text: stats, style: { fg: C.dim } },
  ]);
}

function renderDiffView(): void {
  const v = dv;
  if (!v) return;
  clampView(v);
  renderDiffHeader(v);
  const pw = prefixWidth(v);
  const W = v.codeWidth;
  for (const side of ["old", "new"] as const) {
    const entries: TextPropertyEntry[] = [];
    const regions: TsSyntaxRegion[] = [];
    let byte = 0;
    let regionStart = -1;
    const closeRegion = () => {
      if (regionStart >= 0 && byte > regionStart) {
        regions.push({ start: regionStart, end: byte, language: v.change.path, prefix: pw, streams: [0] });
      }
      regionStart = -1;
    };
    const width = side === "old" ? v.leftWidth : v.rightWidth;
    const lines = side === "old" ? v.oldLines : v.newLines;
    const end = Math.min(v.visual.length, v.top + v.height);
    if (v.visual.length === 0) {
      entries.push({ text: "  No differences\n", style: { fg: C.dim, italic: true } });
    }
    for (let vi = v.top; vi < end; vi++) {
      const vr = v.visual[vi];
      const it = v.items[vr.item];
      const isCur = vi === v.cursor;
      if (it.kind === "fold") {
        closeRegion();
        const text = `${" ".repeat(v.digits)}  ⋯ ${it.to - it.from} unchanged lines${side === "new" ? "  (Enter to expand)" : ""}\n`;
        const style: Style = { fg: C.dim, italic: true, extendToLineEnd: true };
        if (isCur) style.bg = "editor.current_line_bg";
        entries.push({ text, style });
        byte += utf8Len(text);
        continue;
      }
      const r = v.rows[it.row];
      const idx = side === "old" ? r.o : r.n;
      if (idx === null) {
        closeRegion();
        const text = HATCH.repeat(Math.max(1, width - 1)) + "\n";
        entries.push({ text, style: { fg: C.dim } });
        byte += utf8Len(text);
        continue;
      }
      const chunk = lines[idx].slice(vr.part * W, (vr.part + 1) * W);
      const num = vr.part === 0 ? String(idx + 1).padStart(v.digits) : " ".repeat(v.digits);
      const mark = r.chg && vr.part === 0 ? (side === "old" ? "-" : "+") : " ";
      const rowBg = r.chg ? (side === "old" ? "editor.diff_remove_bg" : "editor.diff_add_bg") : isCur ? "editor.current_line_bg" : null;
      const numStyle: Style = { fg: C.dim };
      if (rowBg) numStyle.bg = rowBg;
      if (isCur) {
        numStyle.reversed = true;
        numStyle.bold = true;
      }
      const markStyle: Style = { fg: side === "old" ? C.deleted : C.added, bold: true };
      if (r.chg) markStyle.fg = side === "old" ? "editor.diff_remove_collision_fg" : "editor.diff_add_collision_fg";
      if (rowBg) markStyle.bg = rowBg;
      entries.push({ text: num, style: numStyle });
      entries.push({ text: ` ${mark} `, style: markStyle });
      const code: TextPropertyEntry = { text: chunk + "\n" };
      if (r.chg) {
        code.style = {
          bg: rowBg!,
          extendToLineEnd: true,
          fg: side === "old" ? "editor.diff_remove_collision_fg" : "editor.diff_add_collision_fg",
          fgOnCollisionOnly: true,
        };
        if (r.o !== null && r.n !== null) {
          const ranges = inlineFor(v, it.row)[side === "old" ? 0 : 1];
          const s0 = vr.part * W;
          const s1 = s0 + chunk.length;
          const hl = side === "old" ? "editor.diff_remove_highlight_bg" : "editor.diff_add_highlight_bg";
          const ov: InlineOverlay[] = [];
          for (const [a, b] of ranges) {
            const x = Math.max(a, s0);
            const y = Math.min(b, s1);
            if (y > x) ov.push({ start: x - s0, end: y - s0, style: { bg: hl }, unit: "char" });
          }
          if (ov.length) code.inlineOverlays = ov;
        }
      } else if (rowBg) {
        code.style = { bg: rowBg, extendToLineEnd: true };
      }
      entries.push(code);
      if (regionStart < 0) regionStart = byte;
      byte += pw + utf8Len(chunk + "\n");
    }
    closeRegion();
    editor.setPanelContent(v.groupId, side, entries);
    editor.setSyntaxRegions(v.bufs[side], regions);
  }
}

function changeStarts(v: DiffView): number[] {
  const out: number[] = [];
  v.visual.forEach((vr, i) => {
    const it = v.items[vr.item];
    if (it.kind !== "row" || vr.part !== 0) return;
    const r = v.rows[it.row];
    if (r.chg && (it.row === 0 || !v.rows[it.row - 1].chg)) out.push(i);
  });
  return out;
}

function focusChange(v: DiffView, vi: number): void {
  v.cursor = vi;
  v.top = Math.max(0, vi - Math.floor(v.height / 3));
  renderDiffView();
  const starts = changeStarts(v);
  const k = starts.indexOf(vi);
  if (k >= 0) editor.setStatus(`Change ${k + 1}/${starts.length} · s/u/x stage/unstage/discard it · Enter open file`);
}

async function openDiffView(repo: Repo, c: Change): Promise<void> {
  const abs = editor.pathJoin(repo.root, c.path);
  const v: DiffView = {
    groupId: -1,
    bufs: { hdr: -1, old: -1, new: -1 },
    repo,
    change: c,
    absPath: abs,
    oldLabel: "",
    newLabel: "",
    oldLines: [],
    newLines: [],
    rows: [],
    blocks: [],
    added: 0,
    removed: 0,
    full: false,
    expanded: new Set(),
    items: [],
    visual: [],
    leftWidth: 60,
    rightWidth: 60,
    height: 30,
    digits: 3,
    codeWidth: 50,
    top: 0,
    cursor: 0,
    inline: new Map(),
  };
  const err = await computeDiff(v, await loadDiffPair(repo, c));
  if (err) {
    editor.setStatus(`${c.path}: ${err}`);
    return;
  }
  if (!v.rows.some((r) => r.chg)) {
    editor.setStatus(`${c.path}: no textual changes`);
    return;
  }
  closeDiffView();
  // deno-lint-ignore no-explicit-any
  const group = await (editor as any).createBufferGroup(`*Diff: ${editor.pathBasename(c.path)}*`, "scm-diff-view", DIFF_LAYOUT);
  v.groupId = group.groupId as number;
  v.bufs = { hdr: group.panels["hdr"], old: group.panels["old"], new: group.panels["new"] };
  editor.setBufferShowCursors(v.bufs.old, false);
  editor.setBufferShowCursors(v.bufs.new, false);
  // A first guess at the panel size; the panels report the real one.
  const vp = editor.getViewport();
  if (vp) {
    v.leftWidth = Math.max(20, Math.floor((vp.width - 1) / 2));
    v.rightWidth = Math.max(20, vp.width - 1 - v.leftWidth);
    v.height = Math.max(5, vp.height - 2);
  }
  dv = v;
  relayout(v, null);
  const starts = changeStarts(v);
  focusChange(v, starts[0] ?? 0);
  editor.focusBufferGroupPanel(v.groupId, "new");
  editor.setStatus(
    `${c.path}: +${v.added} -${v.removed} in ${plural(v.blocks.length, "change")} · n/p next/prev · a full file · s/u/x per change · q close`,
  );
}

async function reloadDiffView(): Promise<void> {
  const v = dv;
  if (!v) return;
  const keep = cursorRow(v);
  const err = await computeDiff(v, await loadDiffPair(v.repo, v.change));
  if (dv !== v) return;
  if (err) {
    editor.setStatus(`${v.change.path}: ${err}`);
    return;
  }
  relayout(v, keep);
  renderDiffView();
}

function closeDiffView(): void {
  if (!dv) return;
  const gid = dv.groupId;
  dv = null;
  editor.closeBufferGroup(gid);
}

/** q in the diff view: close it and go back to the Source Control list
 *  (the host would otherwise land on whichever tab sits next to it). */
function scm_dv_close(): void {
  closeDiffView();
  if (state.open && state.groupId !== null) {
    editor.focusBufferGroupPanel(state.groupId, "tree");
    void refresh(false, true);
  }
}

function dvMove(delta: number): void {
  if (!dv) return;
  dv.cursor += delta;
  clampView(dv);
  renderDiffView();
}

function dvPage(dir: 1 | -1): void {
  if (!dv) return;
  const step = Math.max(1, dv.height - 2);
  dv.cursor += dir * step;
  dv.top += dir * step;
  clampView(dv);
  renderDiffView();
}

function dvChange(dir: 1 | -1): void {
  const v = dv;
  if (!v) return;
  const starts = changeStarts(v);
  const next = dir > 0 ? starts.find((i) => i > v.cursor) : [...starts].reverse().find((i) => i < v.cursor);
  if (next === undefined) {
    editor.setStatus(dir > 0 ? "Last change" : "First change");
    return;
  }
  focusChange(v, next);
}

function dvExpandAt(vi: number): boolean {
  const v = dv;
  if (!v) return false;
  const vr = v.visual[vi];
  const it = vr ? v.items[vr.item] : null;
  if (!it || it.kind !== "fold") return false;
  v.expanded.add(it.from);
  relayout(v, null);
  v.cursor = visualForRow(v, it.from);
  clampView(v);
  renderDiffView();
  return true;
}

function dvOpenFile(): void {
  const v = dv;
  if (!v) return;
  if (!editor.fileExists(editor.authorityPath(v.absPath))) {
    editor.setStatus("File does not exist on disk");
    return;
  }
  // The new-side line under the cursor, or the next one that exists.
  let line = 1;
  for (let r = cursorRow(v); r < v.rows.length; r++) {
    const n = v.rows[r].n;
    if (n !== null) {
      line = n + 1;
      break;
    }
  }
  editor.openFile(v.absPath, line, 1);
}

async function dvBlockOp(op: "stage" | "unstage" | "discard"): Promise<void> {
  const v = dv;
  if (!v) return;
  const vr = v.visual[v.cursor];
  const it = vr ? v.items[vr.item] : null;
  const row = it && it.kind === "row" ? v.rows[it.row] : null;
  if (!row || !row.chg || row.block < 0) {
    editor.setStatus("Move the cursor onto a change (n / p) first");
    return;
  }
  const c = v.change;
  if (c.untracked || c.origPath || c.code === "A" || c.code === "D" || c.group === "conflict") {
    editor.setStatus("Per-change staging needs a modified tracked file; use the Source Control list for this one");
    return;
  }
  if ((op === "unstage") !== (c.group === "staged")) {
    editor.setStatus(op === "unstage" ? "These changes are not staged" : "These changes are staged: u unstages them");
    return;
  }
  if (op === "discard" && !(await confirm(`Discard this change in ${c.path}? Cannot be undone`))) {
    editor.setStatus("Discard cancelled");
    return;
  }
  const p = c.path;
  const patch = `diff --git a/${p} b/${p}\n--- a/${p}\n+++ b/${p}\n` + v.blocks[row.block].patch;
  const patchPath = editor.pathJoin(scratchDir(), "change.patch");
  if (!writeTemp(patchPath, patch)) return;
  const args =
    op === "stage" ? ["apply", "--cached", "--unidiff-zero", patchPath]
    : op === "unstage" ? ["apply", "--cached", "--reverse", "--unidiff-zero", patchPath]
    : ["apply", "--reverse", "--unidiff-zero", patchPath];
  const verb = op === "stage" ? "Staged" : op === "unstage" ? "Unstaged" : "Discarded";
  if (await runGitOp(v.repo, args, `${verb} change ${row.block + 1}/${v.blocks.length} of ${p}`)) {
    await reloadDiffView();
    void refresh(false, true);
  }
}

function dvHelp(): void {
  editor.setStatus("j/k ↑/↓ move · PgUp/PgDn · n/p next/prev change · Enter expand ⋯ / open file · a full file · s/u/x stage/unstage/discard change · q close");
}

registerHandler("scm_dv_down", () => dvMove(1));
registerHandler("scm_dv_up", () => dvMove(-1));
registerHandler("scm_dv_pgdn", () => dvPage(1));
registerHandler("scm_dv_pgup", () => dvPage(-1));
registerHandler("scm_dv_home", () => dvMove(-1e9));
registerHandler("scm_dv_end", () => dvMove(1e9));
registerHandler("scm_dv_next", () => dvChange(1));
registerHandler("scm_dv_prev", () => dvChange(-1));
registerHandler("scm_dv_enter", () => {
  if (dv && !dvExpandAt(dv.cursor)) dvOpenFile();
});
registerHandler("scm_dv_open", dvOpenFile);
registerHandler("scm_dv_full", () => {
  if (!dv) return;
  dv.full = !dv.full;
  relayout(dv, cursorRow(dv));
  renderDiffView();
  editor.setStatus(dv.full ? "Showing the whole file (a: changes only)" : "Showing changes only (a: whole file)");
});
registerHandler("scm_dv_stage", () => dvBlockOp("stage"));
registerHandler("scm_dv_unstage", () => dvBlockOp("unstage"));
registerHandler("scm_dv_discard", () => dvBlockOp("discard"));
registerHandler("scm_dv_close", scm_dv_close);
registerHandler("scm_dv_help", dvHelp);
registerHandler("scm_noop", () => {});

editor.defineMode(
  "scm-diff-view",
  [
    ["j", "scm_dv_down"],
    ["Down", "scm_dv_down"],
    ["k", "scm_dv_up"],
    ["Up", "scm_dv_up"],
    ["PageDown", "scm_dv_pgdn"],
    ["Space", "scm_dv_pgdn"],
    ["PageUp", "scm_dv_pgup"],
    ["Home", "scm_dv_home"],
    ["g", "scm_dv_home"],
    ["End", "scm_dv_end"],
    ["G", "scm_dv_end"],
    ["n", "scm_dv_next"],
    ["]", "scm_dv_next"],
    ["p", "scm_dv_prev"],
    ["N", "scm_dv_prev"],
    ["[", "scm_dv_prev"],
    ["Return", "scm_dv_enter"],
    ["o", "scm_dv_open"],
    ["a", "scm_dv_full"],
    ["s", "scm_dv_stage"],
    ["u", "scm_dv_unstage"],
    ["x", "scm_dv_discard"],
    ["?", "scm_dv_help"],
    ["q", "scm_dv_close"],
    ["Escape", "scm_dv_close"],
    ["Left", "scm_noop"],
    ["Right", "scm_noop"],
    ["h", "scm_noop"],
    ["l", "scm_noop"],
    ["C-PageUp", "prev_buffer"],
    ["C-PageDown", "next_buffer"],
  ],
  true, // read-only
  false, // allow_text_input
  true, // inherit Normal-context bindings for unbound keys
);

editor.on("viewport_changed", (d) => {
  const v = dv;
  if (!v || (d.buffer_id !== v.bufs.old && d.buffer_id !== v.bufs.new)) return;
  let changed = false;
  if (d.height > 0 && d.height !== v.height) {
    v.height = d.height;
    changed = true;
  }
  if (d.width > 0) {
    if (d.buffer_id === v.bufs.old && d.width !== v.leftWidth) {
      v.leftWidth = d.width;
      changed = true;
    } else if (d.buffer_id === v.bufs.new && d.width !== v.rightWidth) {
      v.rightWidth = d.width;
      changed = true;
    }
  }
  if (changed) {
    relayout(v, cursorRow(v));
    renderDiffView();
  }
});

editor.on("mouse_scroll", (d) => {
  const v = dv;
  if (!v || (d.buffer_id !== v.bufs.old && d.buffer_id !== v.bufs.new && d.buffer_id !== v.bufs.hdr)) return;
  v.top = Math.max(0, Math.min(v.top + (d.delta > 0 ? 3 : -3), Math.max(0, v.visual.length - v.height)));
  if (v.cursor < v.top) v.cursor = v.top;
  if (v.cursor >= v.top + v.height) v.cursor = v.top + v.height - 1;
  renderDiffView();
});

editor.on("mouse_click", (d) => {
  const v = dv;
  if (!v || (d.buffer_id !== v.bufs.old && d.buffer_id !== v.bufs.new) || d.buffer_row === null) return;
  const vi = v.top + d.buffer_row;
  if (vi >= v.visual.length) return;
  v.cursor = vi;
  if (!dvExpandAt(vi)) renderDiffView();
});

// =============================================================================
// Tree events
// =============================================================================

// A buffer-mounted panel repaints only when its spec is re-sent, so every
// expansion change goes through renderTree().
function toggleRow(row: Row): void {
  if (state.collapsed.has(row.key)) state.collapsed.delete(row.key);
  else state.collapsed.add(row.key);
  renderTree();
}

editor.on("widget_event", (e) => {
  if (!state.open) return;
  if (e.panel_id === TOOLBAR_PANEL) {
    if (e.event_type !== "activate") return;
    const item = toolbarItems().find((it) => `tb.${it.key}` === e.widget_key);
    if (item) void item.run();
    return;
  }
  if (e.panel_id !== TREE_PANEL) return;
  const payload = (e.payload ?? {}) as { index?: unknown; key?: unknown; expanded?: unknown };
  if (e.event_type === "select" || e.event_type === "activate") {
    const idx = typeof payload.index === "number" ? payload.index : -1;
    const row = state.rows[idx];
    if (!row) return;
    const changed = idx !== state.selectedIndex || row.key !== state.selectedKey;
    state.selectedIndex = idx;
    state.selectedKey = row.key;
    if (e.event_type === "activate") void scm_open_diff();
    else if (changed) void updatePreview();
    return;
  }
  if (e.event_type === "action") {
    const idx = typeof payload.index === "number" ? payload.index : -1;
    const row = state.rows[idx];
    if (!row) return;
    state.selectedIndex = idx;
    state.selectedKey = row.key;
    const staged = row.kind === "file" ? row.change.group === "staged"
      : (row.kind === "group" || row.kind === "dir") && row.group === "staged";
    void (staged ? scm_unstage() : scm_stage());
    return;
  }
  if (e.event_type === "expand") {
    if (typeof payload.key !== "string") return;
    const expanded = typeof payload.expanded === "boolean" ? payload.expanded : state.collapsed.has(payload.key);
    if (expanded) state.collapsed.delete(payload.key);
    else state.collapsed.add(payload.key);
    if (typeof payload.index === "number" && state.rows[payload.index]) {
      state.selectedIndex = payload.index;
      state.selectedKey = state.rows[payload.index].key;
    }
    renderTree();
  }
});

// =============================================================================
// Open / close
// =============================================================================

editor.defineMode(
  "source-control",
  [
    ["s", "scm_stage"],
    ["u", "scm_unstage"],
    ["x", "scm_discard"],
    ["c", "scm_commit"],
    ["C", "scm_commit_quick"],
    ["b", "scm_branch"],
    ["o", "scm_open_file"],
    ["p", "scm_pull"],
    ["P", "scm_push"],
    ["f", "scm_fetch"],
    ["r", "scm_refresh"],
    ["h", "scm_toggle_clean"],
    ["t", "scm_toggle_tree"],
    ["j", "scm_down"],
    ["k", "scm_up"],
    ["Home", "scm_first"],
    ["End", "scm_last"],
    ["]", "scm_next_repo"],
    ["[", "scm_prev_repo"],
    ["Return", "scm_detail_enter"],
    ["?", "scm_help"],
    ["Tab", "scm_tab"],
    ["q", "scm_close"],
    // The focused tree would otherwise swallow these as plain PgUp/PgDn.
    ["C-PageUp", "prev_buffer"],
    ["C-PageDown", "next_buffer"],
  ],
  true, // read-only
  false, // allow_text_input
  true, // inherit Normal-context bindings for unbound keys
);

const GROUP_LAYOUT = JSON.stringify({
  type: "split",
  direction: "v",
  ratio: 0.05,
  first: { type: "fixed", id: "toolbar", height: 1 },
  second: {
    type: "split",
    direction: "h",
    ratio: 0.38,
    first: { type: "scrollable", id: "tree", scrollable: false },
    second: { type: "scrollable", id: "detail" },
  },
});

async function scm_open(): Promise<void> {
  if (state.open && state.groupId !== null) {
    editor.focusBufferGroupPanel(state.groupId, "tree");
    void refresh(true, true);
    return;
  }
  state.workspace = editor.getCwd();
  editor.setStatus("Source Control: scanning for repositories…");
  // deno-lint-ignore no-explicit-any
  const group = await (editor as any).createBufferGroup("*Source Control*", "source-control", GROUP_LAYOUT);
  state.groupId = group.groupId as number;
  state.treeBufferId = (group.panels["tree"] as number | undefined) ?? null;
  state.detailBufferId = (group.panels["detail"] as number | undefined) ?? null;
  state.toolbarBufferId = (group.panels["toolbar"] as number | undefined) ?? null;
  state.treeMounted = false;
  state.toolbarMounted = false;
  state.open = true;
  state.signature = "";
  state.roots = [];
  if (state.detailBufferId !== null) {
    editor.setBufferShowCursors(state.detailBufferId, true);
  }
  renderToolbar();
  setDetail([line([["Scanning for repositories…", { fg: C.dim, italic: true }]])]);
  await refresh(true, true);
  editor.focusBufferGroupPanel(state.groupId, "tree");
  startTimer();
  const n = state.repos.length;
  const dirty = state.repos.filter((r) => r.changes.length > 0).length;
  editor.setStatus(`Source Control: ${n} ${n === 1 ? "repository" : "repositories"}, ${dirty} with changes · ? for keys`);
}
registerHandler("scm_open", scm_open);

function cleanup(): void {
  stopTimer();
  state.open = false;
  state.groupId = null;
  state.treeBufferId = null;
  state.detailBufferId = null;
  state.toolbarBufferId = null;
  state.treeMounted = false;
  state.toolbarMounted = false;
  state.rows = [];
  state.repos = [];
  state.signature = "";
}

function scm_close(): void {
  if (!state.open) return;
  const gid = state.groupId;
  cleanup();
  if (gid !== null) editor.closeBufferGroup(gid);
}
registerHandler("scm_close", scm_close);

editor.on("buffer_closed", (data) => {
  if (dv && (data.buffer_id === dv.bufs.old || data.buffer_id === dv.bufs.new || data.buffer_id === dv.bufs.hdr)) {
    dv = null;
  }
  if (state.composite && data.buffer_id === state.composite.compositeId) {
    const cs = state.composite;
    state.composite = null;
    try {
      editor.closeBuffer(cs.oldId);
      editor.closeBuffer(cs.newId);
    } catch {}
  }
  if (state.open && (data.buffer_id === state.treeBufferId || data.buffer_id === state.detailBufferId)) {
    cleanup();
  }
});

editor.on("buffer_activated", (data) => {
  if (state.open && (data.buffer_id === state.treeBufferId || data.buffer_id === state.detailBufferId)) {
    void refresh();
  }
});

editor.on("after_file_save", (data) => {
  if (state.open) void refresh();
  if (dv && dv.change.group !== "staged" && data.path === dv.absPath) void reloadDiffView();
});

editor.on("config_changed", () => {
  if (state.open) {
    startTimer();
    void refresh(true, true);
  }
});

editor.registerCommand(
  "Source Control: Open",
  "VS Code-style view of every git repository under the workspace",
  "scm_open",
  null,
);
editor.registerCommand("Source Control: Refresh", "Re-scan repositories and refresh status", "scm_refresh", null);
