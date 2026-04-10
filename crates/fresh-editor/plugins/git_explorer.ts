/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Git Explorer Decorations
 *
 * Adds VS Code-style status badges (M/A/U/D/...) to the file explorer.
 */

const NAMESPACE = "git-explorer";

// Named color approximations for fallback when theme data is unavailable
const NAMED_COLORS: Record<string, [number, number, number]> = {
  "Black": [0, 0, 0], "Red": [205, 0, 0], "Green": [0, 205, 0],
  "Yellow": [205, 205, 0], "Blue": [0, 0, 238], "Magenta": [205, 0, 205],
  "Cyan": [0, 205, 205], "Gray": [229, 229, 229], "DarkGray": [127, 127, 127],
  "LightRed": [255, 0, 0], "LightGreen": [0, 255, 0], "LightBlue": [92, 92, 255],
  "LightYellow": [255, 255, 0], "LightMagenta": [255, 0, 255],
  "LightCyan": [0, 255, 255], "White": [255, 255, 255],
};

const DEFAULT_COLORS = {
  added: [80, 250, 123] as [number, number, number],
  modified: [255, 184, 108] as [number, number, number],
  deleted: [255, 85, 85] as [number, number, number],
  renamed: [139, 233, 253] as [number, number, number],
  untracked: [241, 250, 140] as [number, number, number],
  conflicted: [255, 121, 198] as [number, number, number],
};

function resolveColor(value: unknown, fallback: [number, number, number]): [number, number, number] {
  if (Array.isArray(value) && value.length === 3
      && typeof value[0] === "number" && typeof value[1] === "number" && typeof value[2] === "number") {
    return value as [number, number, number];
  }
  if (typeof value === "string" && NAMED_COLORS[value]) {
    return NAMED_COLORS[value];
  }
  return fallback;
}

function loadThemeColors(): typeof DEFAULT_COLORS {
  try {
    const config = editor.getConfig() as Record<string, unknown> | null;
    if (!config || !config.theme) return DEFAULT_COLORS;
    const themeData = editor.getThemeData(config.theme as string) as Record<string, unknown> | null;
    if (!themeData) return DEFAULT_COLORS;
    const ui = themeData.ui as Record<string, unknown> | undefined;
    if (!ui) return DEFAULT_COLORS;
    return {
      added:      resolveColor(ui.file_status_added_fg, DEFAULT_COLORS.added),
      modified:   resolveColor(ui.file_status_modified_fg, DEFAULT_COLORS.modified),
      deleted:    resolveColor(ui.file_status_deleted_fg, DEFAULT_COLORS.deleted),
      renamed:    resolveColor(ui.file_status_renamed_fg, DEFAULT_COLORS.renamed),
      untracked:  resolveColor(ui.file_status_untracked_fg, DEFAULT_COLORS.untracked),
      conflicted: resolveColor(ui.file_status_conflicted_fg, DEFAULT_COLORS.conflicted),
    };
  } catch {
    return DEFAULT_COLORS;
  }
}

let COLORS = loadThemeColors();

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
  COLORS = loadThemeColors();
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
  } catch (err) {
    editor.clearFileExplorerDecorations(NAMESPACE);
    throw err;
  } finally {
    refreshInFlight = false;
  }
}

function onGitExplorerAfterFileOpen() {
  refreshGitExplorerDecorations();
}
registerHandler("onGitExplorerAfterFileOpen", onGitExplorerAfterFileOpen);

function onGitExplorerAfterFileSave() {
  refreshGitExplorerDecorations();
}
registerHandler("onGitExplorerAfterFileSave", onGitExplorerAfterFileSave);

function onGitExplorerEditorInitialized() {
  refreshGitExplorerDecorations();
}
registerHandler("onGitExplorerEditorInitialized", onGitExplorerEditorInitialized);

function onGitExplorerFocusGained() {
  refreshGitExplorerDecorations();
}
registerHandler("onGitExplorerFocusGained", onGitExplorerFocusGained);

function onGitExplorerThemesChanged() {
  refreshGitExplorerDecorations();
}
registerHandler("onGitExplorerThemesChanged", onGitExplorerThemesChanged);

editor.on("after_file_open", "onGitExplorerAfterFileOpen");
editor.on("after_file_save", "onGitExplorerAfterFileSave");
editor.on("editor_initialized", "onGitExplorerEditorInitialized");
editor.on("focus_gained", "onGitExplorerFocusGained");
editor.on("themes_changed", "onGitExplorerThemesChanged");

refreshGitExplorerDecorations();
