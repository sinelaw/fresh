/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Git Explorer Decorations
 *
 * Adds VS Code-style status badges (M/A/U/D/...) to the file explorer.
 */

const NAMESPACE = "git-explorer";

const COLORS = {
  added: [80, 250, 123] as [number, number, number],
  modified: [255, 184, 108] as [number, number, number],
  deleted: [255, 85, 85] as [number, number, number],
  renamed: [139, 233, 253] as [number, number, number],
  untracked: [241, 250, 140] as [number, number, number],
  conflicted: [255, 121, 198] as [number, number, number],
};

const PRIORITY = {
  conflicted: 90,
  deleted: 80,
  added: 60,
  modified: 50,
  renamed: 40,
  untracked: 30,
};

let refreshInFlight = false;

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
  const byPath = new Map<string, { path: string; symbol: string; color: [number, number, number]; priority: number }>();

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

async function refreshGitExplorerDecorations() {
  if (refreshInFlight) {
    return;
  }
  refreshInFlight = true;
  try {
    const cwd = editor.getCwd();
    const rootResult = await editor.spawnProcess("git", ["rev-parse", "--show-toplevel"], cwd);
    if (rootResult.exit_code !== 0) {
      editor.clearFileExplorerDecorations(NAMESPACE);
      return;
    }
    const repoRoot = rootResult.stdout.trim();
    if (!repoRoot) {
      editor.clearFileExplorerDecorations(NAMESPACE);
      return;
    }

    const statusResult = await editor.spawnProcess(
      "git",
      ["status", "--porcelain"],
      repoRoot
    );
    if (statusResult.exit_code !== 0) {
      editor.clearFileExplorerDecorations(NAMESPACE);
      return;
    }

    const decorations = parseStatusOutput(statusResult.stdout, repoRoot);
    if (decorations.length === 0) {
      editor.clearFileExplorerDecorations(NAMESPACE);
    } else {
      editor.setFileExplorerDecorations(NAMESPACE, decorations);
    }
  } catch (_err) {
    editor.clearFileExplorerDecorations(NAMESPACE);
  } finally {
    refreshInFlight = false;
  }
}

globalThis.onGitExplorerAfterFileOpen = () => {
  refreshGitExplorerDecorations();
};

globalThis.onGitExplorerAfterFileSave = () => {
  refreshGitExplorerDecorations();
};

globalThis.onGitExplorerEditorInitialized = () => {
  refreshGitExplorerDecorations();
};

editor.on("after_file_open", "onGitExplorerAfterFileOpen");
editor.on("after_file_save", "onGitExplorerAfterFileSave");
editor.on("editor_initialized", "onGitExplorerEditorInitialized");

refreshGitExplorerDecorations();
