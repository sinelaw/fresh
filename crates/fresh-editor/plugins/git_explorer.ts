/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Git Explorer Decorations
 *
 * Adds VS Code-style status badges (M/A/U/D/...) to the file explorer.
 * Optional filename coloring via plugins.git_explorer.settings.colorNames.
 */

const NAMESPACE = "git-explorer";

const COLORS = {
  added:      "ui.file_status_added_fg",
  modified:   "ui.file_status_modified_fg",
  deleted:    "ui.file_status_deleted_fg",
  renamed:    "ui.file_status_renamed_fg",
  untracked:  "ui.file_status_untracked_fg",
  conflicted: "ui.file_status_conflicted_fg",
};

const PRIORITY = {
  conflicted: 90,
  deleted: 80,
  added: 60,
  modified: 50,
  renamed: 40,
  untracked: 30,
};

editor.defineConfigBoolean("colorNames", {
  default: false,
  description: "Color file explorer entry names by git status",
});

let refreshInFlight = false;
let refreshPending = false;

function statusToDecoration(status: string, staged: boolean) {
  switch (status) {
    case "A":
      return { symbol: "A", color: COLORS.added, priority: PRIORITY.added };
    case "M":
      return {
        symbol: "M",
        color: staged ? COLORS.added : COLORS.modified,
        priority: PRIORITY.modified + (staged ? 2 : 0),
      };
    case "D":
      return { symbol: "D", color: COLORS.deleted, priority: PRIORITY.deleted };
    case "R":
      return { symbol: "R", color: COLORS.renamed, priority: PRIORITY.renamed };
    case "C":
      return { symbol: "C", color: COLORS.renamed, priority: PRIORITY.renamed };
    case "U":
      return { symbol: "!", color: COLORS.conflicted, priority: PRIORITY.conflicted };
    default:
      return null;
  }
}

function parseStatusOutput(output: string, repoRoot: string) {
  const separator = output.includes("\0") ? "\0" : "\n";
  const entries = output
    .split(separator)
    .map((entry) => entry.replace(/\r$/, ""))
    .filter((entry) => entry.length > 0);
  const byPath = new Map<string, { path: string; symbol: string; color: string; priority: number }>();

  for (let i = 0; i < entries.length; i++) {
    const entry = entries[i];
    if (entry.length < 3) {
      continue;
    }
    const x = entry[0];
    const y = entry[1];
    let path = entry.slice(3);

    if ((x === "R" || x === "C") && separator === "\0" && i + 1 < entries.length) {
      i += 1;
      path = entries[i];
    } else if (entry.includes(" -> ") && (x === "R" || x === "C" || y === "R" || y === "C")) {
      path = entry.split(" -> ").pop() ?? path;
    }

    let decoration = null;
    if (x === "?" && y === "?") {
      decoration = { symbol: "U", color: COLORS.untracked, priority: PRIORITY.untracked };
    } else if (x !== " " && x !== "?") {
      decoration = statusToDecoration(x, true);
    } else if (y !== " ") {
      decoration = statusToDecoration(y, false);
    }

    if (!decoration) {
      continue;
    }

    const absolutePath = editor.pathJoin(repoRoot, path);
    const existing = byPath.get(absolutePath);
    if (!existing || decoration.priority >= existing.priority) {
      byPath.set(absolutePath, { path: absolutePath, ...decoration });
    }
  }

  return Array.from(byPath.values());
}

function buildNameColorSlots(
  statuses: Array<{ path: string; color: string; priority: number }>,
  repoRoot: string
) {
  const byPath = new Map<string, { path: string; nameColor: string; priority: number }>();

  for (const status of statuses) {
    const existing = byPath.get(status.path);
    if (!existing || status.priority >= existing.priority) {
      byPath.set(status.path, {
        path: status.path,
        nameColor: status.color,
        priority: status.priority,
      });
    }

    let ancestor = status.path;
    while (true) {
      const parent = editor.pathDirname(ancestor);
      if (!parent || parent === ancestor || !parent.startsWith(repoRoot)) {
        break;
      }
      const bubbled = byPath.get(parent);
      if (!bubbled || status.priority >= bubbled.priority) {
        byPath.set(parent, {
          path: parent,
          nameColor: status.color,
          priority: status.priority,
        });
      }
      ancestor = parent;
    }
  }

  return Array.from(byPath.values()).map(({ path, nameColor, priority }) => ({
    path,
    nameColor,
    priority,
  }));
}

/**
 * Recursively discover directories containing `.git` entries, up to maxDepth
 * levels. Stops descending into a directory once `.git` is found (git repo
 * internals like submodules are managed by git itself).
 */
function discoverSubRepos(dir: string, maxDepth: number = 3): string[] {
  if (maxDepth <= 0) return [];
  const repos: string[] = [];
  const entries = editor.readDir(dir);
  for (const entry of entries) {
    if (entry.name.startsWith(".") || entry.name === "node_modules" || !entry.is_dir) continue;
    const subDir = editor.pathJoin(dir, entry.name);
    if (editor.fileExists(editor.pathJoin(subDir, ".git"))) {
      repos.push(subDir);
    } else {
      repos.push(...discoverSubRepos(subDir, maxDepth - 1));
    }
  }
  return repos;
}
async function refreshGitExplorerDecorations() {
  if (refreshInFlight) {
    refreshPending = true;
    return;
  }
  refreshInFlight = true;
  try {
    const cwd = editor.getCwd();
    const rootResult = await editor.spawnProcess("git", ["rev-parse", "--show-toplevel"], cwd);

    let allDecorations: Array<{ path: string; symbol: string; color: string; priority: number }> = [];
    let allRepoRoots: string[] = [];

    if (rootResult.exit_code === 0 && rootResult.stdout.trim()) {
      // cwd is inside a git repo — single-repo mode
      const repoRoot = rootResult.stdout.trim();
      allRepoRoots = [repoRoot];
      const statusResult = await editor.spawnProcess("git", ["status", "--porcelain", "-z"], repoRoot);
      if (statusResult.exit_code === 0) {
        allDecorations = parseStatusOutput(statusResult.stdout, repoRoot);
      }
    } else {
      // cwd is not a git repo — discover sub-repos (monorepo/multi-repo)
      const subRepos = discoverSubRepos(cwd);
      for (const subDir of subRepos) {
        const subRootResult = await editor.spawnProcess("git", ["rev-parse", "--show-toplevel"], subDir);
        if (subRootResult.exit_code !== 0) continue;
        const repoRoot = subRootResult.stdout.trim();
        if (!repoRoot) continue;
        allRepoRoots.push(repoRoot);
        const statusResult = await editor.spawnProcess("git", ["status", "--porcelain", "-z"], repoRoot);
        if (statusResult.exit_code === 0) {
          allDecorations = allDecorations.concat(parseStatusOutput(statusResult.stdout, repoRoot));
        }
      }
    }

    if (allDecorations.length === 0) {
      editor.clearFileExplorerDecorations(NAMESPACE);
      editor.clearFileExplorerSlots(NAMESPACE);
    } else {
      editor.setFileExplorerDecorations(NAMESPACE, allDecorations);

      const cfg = (editor.getPluginConfig() ?? {}) as { colorNames?: boolean };
      if (cfg.colorNames) {
        let allSlots: Array<{ path: string; nameColor: string; priority: number }> = [];
        for (const repoRoot of allRepoRoots) {
          const repoDecorations = allDecorations.filter(d => d.path.startsWith(repoRoot));
          allSlots = allSlots.concat(buildNameColorSlots(repoDecorations, repoRoot));
        }
        editor.setFileExplorerSlots(NAMESPACE, allSlots);
      } else {
        editor.clearFileExplorerSlots(NAMESPACE);
      }
    }
  } catch (err) {
    editor.clearFileExplorerDecorations(NAMESPACE);
    editor.clearFileExplorerSlots(NAMESPACE);
    throw err;
  } finally {
    refreshInFlight = false;
    if (refreshPending) {
      refreshPending = false;
      void refreshGitExplorerDecorations();
    }
  }
}

editor.on("after_file_open", () => {
  refreshGitExplorerDecorations();
});
editor.on("after_file_save", () => {
  refreshGitExplorerDecorations();
});
editor.on("after_file_explorer_change", () => {
  refreshGitExplorerDecorations();
});
editor.on("editor_initialized", () => {
  refreshGitExplorerDecorations();
});
editor.on("focus_gained", () => {
  refreshGitExplorerDecorations();
});

refreshGitExplorerDecorations();
