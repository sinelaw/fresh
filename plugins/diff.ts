/// <reference path="../types/fresh.d.ts" />

/**
 * Diff Plugin
 *
 * Provides comprehensive diff functionality:
 * - Diff two directories
 * - Diff local with remote
 * - Diff local with branch
 * - Diff local with commit or tag
 * - Diff two different commits
 * - Click on diff lines to show file differences
 *
 * Usage:
 * - show_directory_diff: Compare two directories
 * - show_git_diff: Show git diff (local vs remote/branch/commit)
 * - show_commit_diff: Compare two commits
 */

// =============================================================================
// Constants
// =============================================================================

const NAMESPACE = "diff";
const MODE_DIFF_LIST = "diff-list";
const MODE_DIFF_VIEW = "diff-view";

// Colors (RGB)
const COLORS = {
  added: [80, 250, 123] as [number, number, number],      // Green
  deleted: [255, 85, 85] as [number, number, number],   // Red
  modified: [255, 184, 108] as [number, number, number], // Orange/Yellow
  header: [139, 233, 253] as [number, number, number],  // Cyan
  context: [173, 173, 173] as [number, number, number],  // Gray
};

// =============================================================================
// Types
// =============================================================================

interface DiffFile {
  path: string;
  status: "added" | "deleted" | "modified" | "renamed" | "copied";
  oldPath?: string;
  additions?: number;
  deletions?: number;
}

interface DiffState {
  isOpen: boolean;
  bufferId: number | null;
  leftBufferId: number | null;
  rightBufferId: number | null;
  splitId: number | null;
  leftSplitId: number | null;
  rightSplitId: number | null;
  sourceBufferId: number | null;
  files: DiffFile[];
  diffType: "directory" | "git" | "commit";
  leftPath: string;
  rightPath: string;
  currentFile: string | null;
  unified: boolean; // true for unified, false for side-by-side
  currentViewMode: "list" | "unified" | "side-by-side";
}

// =============================================================================
// State
// =============================================================================

const diffState: DiffState = {
  isOpen: false,
  bufferId: null,
  leftBufferId: null,
  rightBufferId: null,
  splitId: null,
  leftSplitId: null,
  rightSplitId: null,
  sourceBufferId: null,
  files: [],
  diffType: "directory",
  leftPath: "",
  rightPath: "",
  currentFile: null,
  unified: true,
  currentViewMode: "list",
};

// =============================================================================
// Git Operations
// =============================================================================

/**
 * Get git repository root
 */
async function getGitRoot(cwd: string): Promise<string | null> {
  try {
    const result = await editor.spawnProcess("git", ["rev-parse", "--show-toplevel"], cwd);
    if (result.exit_code === 0) {
      return result.stdout.trim();
    }
  } catch (e) {
    editor.debug(`getGitRoot error: ${e}`);
  }
  return null;
}

/**
 * Check if path is in a git repository
 */
async function isGitRepo(path: string): Promise<boolean> {
  const dir = editor.pathDirname(path);
  const root = await getGitRoot(dir);
  return root !== null;
}

/**
 * Get git diff between two paths (commits, branches, etc.)
 */
async function getGitDiff(
  left: string,
  right: string,
  cwd: string
): Promise<string> {
  try {
    const result = await editor.spawnProcess("git", ["diff", left, right], cwd);
    if (result.exit_code === 0) {
      return result.stdout;
    }
  } catch (e) {
    editor.debug(`getGitDiff error: ${e}`);
  }
  return "";
}

/**
 * Get git diff for a specific file
 */
async function getGitDiffFile(
  left: string,
  right: string,
  filePath: string,
  cwd: string
): Promise<string> {
  try {
    const result = await editor.spawnProcess(
      "git",
      ["diff", left, right, "--", filePath],
      cwd
    );
    if (result.exit_code === 0) {
      return result.stdout;
    }
  } catch (e) {
    editor.debug(`getGitDiffFile error: ${e}`);
  }
  return "";
}

/**
 * Get list of changed files between two refs
 */
async function getGitDiffFiles(
  left: string,
  right: string,
  cwd: string
): Promise<DiffFile[]> {
  try {
    // Use --name-status to get file changes
    const result = await editor.spawnProcess(
      "git",
      ["diff", "--name-status", left, right],
      cwd
    );
    if (result.exit_code === 0) {
      return parseGitDiffStatus(result.stdout);
    }
  } catch (e) {
    editor.debug(`getGitDiffFiles error: ${e}`);
  }
  return [];
}

/**
 * Parse git diff --name-status output
 */
function parseGitDiffStatus(output: string): DiffFile[] {
  const files: DiffFile[] = [];
  const lines = output.split("\n").filter((line) => line.trim());

  for (const line of lines) {
    const match = line.match(/^([AMD]|R\d+|C\d+)\s+(.+?)(?:\s+(.+))?$/);
    if (match) {
      const statusCode = match[1];
      const path1 = match[2];
      const path2 = match[3];

      let status: DiffFile["status"] = "modified";
      let oldPath: string | undefined;

      if (statusCode === "A") {
        status = "added";
      } else if (statusCode === "D") {
        status = "deleted";
      } else if (statusCode.startsWith("R")) {
        status = "renamed";
        oldPath = path1;
      } else if (statusCode.startsWith("C")) {
        status = "copied";
        oldPath = path1;
      }

      files.push({
        path: path2 || path1,
        status,
        oldPath: oldPath || undefined,
      });
    }
  }

  return files;
}

/**
 * Get git diff stats (additions/deletions) for a file
 */
async function getGitDiffStats(
  left: string,
  right: string,
  filePath: string,
  cwd: string
): Promise<{ additions: number; deletions: number }> {
  try {
    const result = await editor.spawnProcess(
      "git",
      ["diff", "--numstat", left, right, "--", filePath],
      cwd
    );
    if (result.exit_code === 0) {
      const match = result.stdout.trim().match(/^(\d+)\s+(\d+)/);
      if (match) {
        return {
          additions: parseInt(match[1], 10),
          deletions: parseInt(match[2], 10),
        };
      }
    }
  } catch (e) {
    editor.debug(`getGitDiffStats error: ${e}`);
  }
  return { additions: 0, deletions: 0 };
}

// =============================================================================
// Directory Diff Operations
// =============================================================================

/**
 * Compare two directories and return list of different files
 */
async function compareDirectories(
  leftDir: string,
  rightDir: string
): Promise<DiffFile[]> {
  const files: DiffFile[] = [];

  try {
    // Get all files from both directories recursively
    const leftFiles = await getAllFiles(leftDir);
    const rightFiles = await getAllFiles(rightDir);

    const leftSet = new Set(leftFiles);
    const rightSet = new Set(rightFiles);

    // Find added files (in right but not in left)
    for (const file of rightFiles) {
      if (!leftSet.has(file)) {
        files.push({ path: file, status: "added" });
      }
    }

    // Find deleted files (in left but not in right)
    for (const file of leftFiles) {
      if (!rightSet.has(file)) {
        files.push({ path: file, status: "deleted" });
      }
    }

    // Find modified files (in both but different)
    for (const file of leftFiles) {
      if (rightSet.has(file)) {
        const leftPath = editor.pathJoin(leftDir, file);
        const rightPath = editor.pathJoin(rightDir, file);
        if (await filesDiffer(leftPath, rightPath)) {
          files.push({ path: file, status: "modified" });
        }
      }
    }
  } catch (e) {
    editor.debug(`compareDirectories error: ${e}`);
  }

  return files;
}

/**
 * Get all files in a directory recursively
 */
async function getAllFiles(dir: string): Promise<string[]> {
  const files: string[] = [];

  try {
    const entries = editor.readDir(dir);
    for (const entry of entries) {
      const fullPath = editor.pathJoin(dir, entry.name);
      if (entry.is_file) {
        files.push(entry.name);
      } else if (entry.is_directory) {
        const subFiles = await getAllFiles(fullPath);
        for (const subFile of subFiles) {
          files.push(editor.pathJoin(entry.name, subFile));
        }
      }
    }
  } catch (e) {
    editor.debug(`getAllFiles error: ${e}`);
  }

  return files;
}

/**
 * Check if two files differ
 */
async function filesDiffer(leftPath: string, rightPath: string): Promise<boolean> {
  try {
    const leftContent = await editor.readFile(leftPath);
    const rightContent = await editor.readFile(rightPath);
    return leftContent !== rightContent;
  } catch (e) {
    // If either file can't be read, consider them different
    return true;
  }
}

/**
 * Get unified diff between two files
 */
async function getFileDiff(
  leftPath: string,
  rightPath: string
): Promise<string> {
  try {
    // Use system diff command if available
    const result = await editor.spawnProcess("diff", ["-u", leftPath, rightPath]);
    if (result.exit_code <= 1) {
      // diff returns 0 if identical, 1 if different, 2 if error
      return result.stdout;
    }
  } catch (e) {
    editor.debug(`getFileDiff error: ${e}`);
  }

  // Fallback: simple line-by-line comparison
  try {
    const leftContent = await editor.readFile(leftPath);
    const rightContent = await editor.readFile(rightPath);
    return generateSimpleDiff(leftContent, rightContent, leftPath, rightPath);
  } catch (e) {
    return `Error reading files: ${e}`;
  }
}

/**
 * Generate a simple unified diff
 */
function generateSimpleDiff(
  left: string,
  right: string,
  leftPath: string,
  rightPath: string
): string {
  const leftLines = left.split("\n");
  const rightLines = right.split("\n");
  const diff: string[] = [];

  diff.push(`--- ${leftPath}`);
  diff.push(`+++ ${rightPath}`);

  // Simple line-by-line diff
  const maxLen = Math.max(leftLines.length, rightLines.length);
  for (let i = 0; i < maxLen; i++) {
    const leftLine = leftLines[i];
    const rightLine = rightLines[i];

    if (leftLine === undefined) {
      diff.push(`+${rightLine}`);
    } else if (rightLine === undefined) {
      diff.push(`-${leftLine}`);
    } else if (leftLine !== rightLine) {
      diff.push(`-${leftLine}`);
      diff.push(`+${rightLine}`);
    } else {
      diff.push(` ${leftLine}`);
    }
  }

  return diff.join("\n");
}

// =============================================================================
// Diff Display
// =============================================================================

/**
 * Build file list entries for diff view
 */
function buildFileListEntries(files: DiffFile[]): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Header
  entries.push({
    text: `Diff: ${diffState.leftPath} ↔ ${diffState.rightPath}\n`,
    properties: { type: "header" },
  });

  entries.push({
    text: `${files.length} file(s) changed\n\n`,
    properties: { type: "header" },
  });

  // File list
  for (let i = 0; i < files.length; i++) {
    const file = files[i];
    const statusSymbol = getStatusSymbol(file.status);
    const statusText = file.status.toUpperCase().padEnd(8);

    entries.push({
      text: `${statusSymbol} ${statusText} ${file.path}`,
      properties: {
        type: "file",
        file: file.path,
        status: file.status,
        index: i,
      },
    });

    if (file.additions !== undefined || file.deletions !== undefined) {
      const stats = ` (+${file.additions || 0}/-${file.deletions || 0})`;
      entries.push({
        text: stats,
        properties: { type: "stats" },
      });
    }

    entries.push({ text: "\n", properties: {} });
  }

  entries.push({
    text: "\nPress Enter on a file to view its diff | q to quit\n",
    properties: { type: "help" },
  });

  return entries;
}

/**
 * Get status symbol for file change type
 */
function getStatusSymbol(status: DiffFile["status"]): string {
  switch (status) {
    case "added":
      return "+";
    case "deleted":
      return "-";
    case "modified":
      return "~";
    case "renamed":
      return "→";
    case "copied":
      return "=";
    default:
      return "?";
  }
}

/**
 * Parse unified diff into left and right file contents with alignment
 */
interface SideBySideLine {
  leftLine: string | null;  // null for added lines
  rightLine: string | null; // null for deleted lines
  leftLineNum: number | null;
  rightLineNum: number | null;
  type: "context" | "added" | "deleted" | "modified";
}

function parseUnifiedDiffForSideBySide(diffText: string): {
  leftLines: SideBySideLine[];
  rightLines: SideBySideLine[];
  leftPath: string;
  rightPath: string;
} {
  const lines = diffText.split("\n");
  const result: SideBySideLine[] = [];
  
  let leftPath = "";
  let rightPath = "";
  let leftLineNum = 0;
  let rightLineNum = 0;
  let inHunk = false;

  for (const line of lines) {
    if (line.startsWith("---")) {
      leftPath = line.substring(4).trim();
      // Remove timestamp if present: "a/file.txt\t2024-01-01 12:00:00"
      const tabIndex = leftPath.indexOf("\t");
      if (tabIndex >= 0) {
        leftPath = leftPath.substring(0, tabIndex);
      }
      continue;
    }
    if (line.startsWith("+++")) {
      rightPath = line.substring(4).trim();
      // Remove timestamp if present
      const tabIndex = rightPath.indexOf("\t");
      if (tabIndex >= 0) {
        rightPath = rightPath.substring(0, tabIndex);
      }
      continue;
    }
    if (line.startsWith("@@")) {
      // Parse hunk header: @@ -start,count +start,count @@
      const match = line.match(/@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@/);
      if (match) {
        leftLineNum = parseInt(match[1], 10);
        rightLineNum = parseInt(match[3], 10);
        inHunk = true;
      }
      continue;
    }

    if (!inHunk) continue;

    // Handle empty lines and different line types
    if (line === "") {
      // Empty line - treat as context
      result.push({
        leftLine: "",
        rightLine: "",
        leftLineNum: leftLineNum++,
        rightLineNum: rightLineNum++,
        type: "context",
      });
    } else if (line.startsWith("+") && !line.startsWith("++")) {
      // Added line (only in right)
      result.push({
        leftLine: null,
        rightLine: line.substring(1),
        leftLineNum: null,
        rightLineNum: rightLineNum++,
        type: "added",
      });
    } else if (line.startsWith("-") && !line.startsWith("--")) {
      // Deleted line (only in left)
      result.push({
        leftLine: line.substring(1),
        rightLine: null,
        leftLineNum: leftLineNum++,
        rightLineNum: null,
        type: "deleted",
      });
    } else if (line.startsWith("\\")) {
      // End of file marker (No newline at end of file)
      continue;
    } else {
      // Context line (in both) - starts with space or is regular line
      const content = line.startsWith(" ") ? line.substring(1) : line;
      result.push({
        leftLine: content,
        rightLine: content,
        leftLineNum: leftLineNum++,
        rightLineNum: rightLineNum++,
        type: "context",
      });
    }
  }

  return {
    leftLines: result,
    rightLines: result, // Same array, different rendering
    leftPath,
    rightPath,
  };
}

/**
 * Build unified diff view entries
 */
function buildUnifiedDiffEntries(diffText: string, filePath: string): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];
  const lines = diffText.split("\n");

  // Header
  entries.push({
    text: `Diff: ${filePath}\n`,
    properties: { type: "header" },
  });
  entries.push({ text: "\n", properties: {} });

  for (const line of lines) {
    if (line.startsWith("+++") || line.startsWith("---")) {
      entries.push({
        text: line + "\n",
        properties: { type: "header" },
      });
    } else if (line.startsWith("@@")) {
      entries.push({
        text: line + "\n",
        properties: { type: "context" },
      });
    } else if (line.startsWith("+")) {
      entries.push({
        text: line + "\n",
        properties: { type: "added" },
      });
    } else if (line.startsWith("-")) {
      entries.push({
        text: line + "\n",
        properties: { type: "deleted" },
      });
    } else {
      entries.push({
        text: line + "\n",
        properties: { type: "context" },
      });
    }
  }

  entries.push({
    text: "\nPress q to go back | s to toggle side-by-side\n",
    properties: { type: "help" },
  });

  return entries;
}

/**
 * Build side-by-side diff entries for left buffer
 */
function buildSideBySideLeftEntries(parsed: { leftLines: SideBySideLine[]; leftPath: string }): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];
  
  entries.push({
    text: `${parsed.leftPath}\n`,
    properties: { type: "header" },
  });
  entries.push({ text: "\n", properties: {} });

  for (const line of parsed.leftLines) {
    if (line.leftLine === null) {
      // Added line - show blank
      entries.push({
        text: "\n",
        properties: { type: "added", lineNum: null },
      });
    } else {
      const lineNum = line.leftLineNum !== null ? `${line.leftLineNum}`.padStart(6) + " " : "       ";
      entries.push({
        text: `${lineNum}${line.leftLine}\n`,
        properties: {
          type: line.type,
          lineNum: line.leftLineNum,
        },
      });
    }
  }

  return entries;
}

/**
 * Build side-by-side diff entries for right buffer
 */
function buildSideBySideRightEntries(parsed: { rightLines: SideBySideLine[]; rightPath: string }): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];
  
  entries.push({
    text: `${parsed.rightPath}\n`,
    properties: { type: "header" },
  });
  entries.push({ text: "\n", properties: {} });

  for (const line of parsed.rightLines) {
    if (line.rightLine === null) {
      // Deleted line - show blank
      entries.push({
        text: "\n",
        properties: { type: "deleted", lineNum: null },
      });
    } else {
      const lineNum = line.rightLineNum !== null ? `${line.rightLineNum}`.padStart(6) + " " : "       ";
      entries.push({
        text: `${lineNum}${line.rightLine}\n`,
        properties: {
          type: line.type,
          lineNum: line.rightLineNum,
        },
      });
    }
  }

  return entries;
}

/**
 * Apply syntax highlighting to side-by-side diff view
 * Note: This is a simplified version. For full highlighting, we'd need to
 * read the buffer content and calculate exact byte offsets.
 */
function applySideBySideHighlighting(
  leftBufferId: number,
  rightBufferId: number,
  lines: SideBySideLine[]
): void {
  // Clear existing overlays
  editor.clearNamespace(leftBufferId, NAMESPACE);
  editor.clearNamespace(rightBufferId, NAMESPACE);

  // For now, we'll apply highlighting based on text properties
  // The actual byte offset calculation would require reading the buffer
  // This is a placeholder - full implementation would need to:
  // 1. Read buffer content
  // 2. Calculate byte offsets for each line
  // 3. Apply overlays at correct positions
  
  // TODO: Implement full byte offset calculation
  // For now, the text properties in the entries provide the type information
  // which can be used by a more sophisticated highlighting system
}

/**
 * Apply syntax highlighting to unified diff view
 */
function applyDiffHighlighting(bufferId: number): void {
  const bufferLength = editor.getBufferLength(bufferId);
  if (bufferLength === 0) return;

  // Clear existing overlays
  editor.clearNamespace(bufferId, NAMESPACE);

  // This would need to read the buffer content and apply highlighting
  // For now, we rely on text properties set in buildUnifiedDiffEntries
  // TODO: Implement full highlighting based on text properties
}

// =============================================================================
// Public Commands
// =============================================================================

// =============================================================================
// Prompt State
// =============================================================================

interface PromptState {
  type: "directory-left" | "directory-right" | "git-right" | "commit-left" | "commit-right" | null;
  leftValue: string;
  rightValue: string;
}

const promptState: PromptState = {
  type: null,
  leftValue: "",
  rightValue: "",
};

/**
 * Show directory diff
 */
globalThis.show_directory_diff = function(): void {
  if (diffState.isOpen) {
    editor.setStatus("Diff view already open");
    return;
  }

  promptState.type = "directory-left";
  promptState.leftValue = "";
  promptState.rightValue = "";

  editor.startPrompt("Left directory:", "diff-directory");
  editor.setStatus("Enter left directory path");
};

/**
 * Show git diff (local vs remote/branch/commit)
 */
globalThis.show_git_diff = async function(): Promise<void> {
  if (diffState.isOpen) {
    editor.setStatus("Diff view already open");
    return;
  }

  const bufferId = editor.getActiveBufferId();
  const filePath = editor.getBufferPath(bufferId);
  if (!filePath) {
    editor.setStatus("No file open");
    return;
  }

  const cwd = editor.pathDirname(filePath);
  const gitRoot = await getGitRoot(cwd);
  if (!gitRoot) {
    editor.setStatus("Not in a git repository");
    return;
  }

  promptState.type = "git-right";
  promptState.leftValue = "";
  promptState.rightValue = "";

  editor.startPrompt("Compare with (branch/commit/tag/remote, empty for HEAD):", "diff-git");
  editor.setStatus("Enter git ref to compare with working directory");
};

/**
 * Show commit diff (compare two commits)
 */
globalThis.show_commit_diff = function(): void {
  if (diffState.isOpen) {
    editor.setStatus("Diff view already open");
    return;
  }

  const bufferId = editor.getActiveBufferId();
  const filePath = editor.getBufferPath(bufferId);
  if (!filePath) {
    editor.setStatus("No file open");
    return;
  }

  promptState.type = "commit-left";
  promptState.leftValue = "";
  promptState.rightValue = "";

  editor.startPrompt("Left commit/branch/tag:", "diff-commit");
  editor.setStatus("Enter left git ref");
};

/**
 * Show diff list view
 */
async function showDiffList(): Promise<void> {
  diffState.splitId = editor.getActiveSplitId();
  diffState.sourceBufferId = editor.getActiveBufferId();

  const entries = buildFileListEntries(diffState.files);

  const bufferId = await editor.createVirtualBufferInExistingSplit({
    name: "*Diff*",
    mode: MODE_DIFF_LIST,
    read_only: true,
    entries: entries,
    split_id: diffState.splitId!,
    show_line_numbers: false,
    show_cursors: true,
    editing_disabled: true,
  });

  if (bufferId !== null) {
    diffState.isOpen = true;
    diffState.bufferId = bufferId;
    editor.setStatus(`Diff: ${diffState.files.length} files | Enter: view diff | q: quit`);
  }
}

/**
 * Show file diff view (unified or side-by-side)
 */
async function showFileDiff(filePath: string, useSideBySide: boolean = false): Promise<void> {
  diffState.currentFile = filePath;
  diffState.currentViewMode = useSideBySide ? "side-by-side" : "unified";

  let diffText = "";

  if (diffState.diffType === "directory") {
    const leftPath = editor.pathJoin(diffState.leftPath, filePath);
    const rightPath = editor.pathJoin(diffState.rightPath, filePath);
    diffText = await getFileDiff(leftPath, rightPath);
  } else if (diffState.diffType === "git" || diffState.diffType === "commit") {
    const bufferId = editor.getActiveBufferId();
    const currentPath = editor.getBufferPath(bufferId);
    const cwd = currentPath ? editor.pathDirname(currentPath) : editor.getCwd();
    const gitRoot = await getGitRoot(cwd);
    if (gitRoot) {
      diffText = await getGitDiffFile(
        diffState.leftPath || "HEAD",
        diffState.rightPath,
        filePath,
        gitRoot
      );
    }
  }

  if (useSideBySide) {
    await showSideBySideDiff(diffText, filePath);
  } else {
    const entries = buildUnifiedDiffEntries(diffText, filePath);
    if (diffState.bufferId !== null) {
      editor.setVirtualBufferContent(diffState.bufferId, entries);
      editor.setStatus(`Diff: ${filePath} | q: back | s: side-by-side`);
    }
  }
}

/**
 * Show side-by-side diff view
 */
async function showSideBySideDiff(diffText: string, filePath: string): Promise<void> {
  const parsed = parseUnifiedDiffForSideBySide(diffText);
  
  // Close unified view if open
  if (diffState.bufferId !== null && diffState.currentViewMode === "unified") {
    editor.closeBuffer(diffState.bufferId);
    diffState.bufferId = null;
  }

  // Get source split to create side-by-side from
  const sourceSplitId = diffState.splitId || editor.getActiveSplitId();

  // Create left buffer (old version)
  const leftEntries = buildSideBySideLeftEntries({
    leftLines: parsed.leftLines,
    leftPath: parsed.leftPath || filePath,
  });

  const leftBufferId = await editor.createVirtualBufferInExistingSplit({
    name: `*LEFT: ${editor.pathBasename(filePath)}*`,
    mode: MODE_DIFF_VIEW,
    read_only: true,
    entries: leftEntries,
    split_id: sourceSplitId,
    show_line_numbers: false,
    show_cursors: true,
    editing_disabled: true,
  });

  if (leftBufferId === null) {
    editor.setStatus("Failed to create left diff buffer");
    return;
  }

  diffState.leftBufferId = leftBufferId;
  diffState.leftSplitId = editor.getActiveSplitId();

  // Create right buffer (new version) in vertical split
  const rightEntries = buildSideBySideRightEntries({
    rightLines: parsed.rightLines,
    rightPath: parsed.rightPath || filePath,
  });

  const rightBufferId = await editor.createVirtualBufferInSplit({
    name: `*RIGHT: ${editor.pathBasename(filePath)}*`,
    mode: MODE_DIFF_VIEW,
    read_only: true,
    entries: rightEntries,
    ratio: 0.5,
    direction: "vertical",
    show_line_numbers: false,
    show_cursors: true,
    editing_disabled: true,
  });

  if (rightBufferId === null) {
    editor.setStatus("Failed to create right diff buffer");
    return;
  }

  diffState.rightBufferId = rightBufferId;
  diffState.rightSplitId = editor.getActiveSplitId();

  // Apply highlighting to both buffers
  applySideBySideHighlighting(leftBufferId, rightBufferId, parsed.leftLines);

  // Distribute splits evenly
  editor.distributeSplitsEvenly();

  editor.setStatus(`Side-by-side: ${filePath} | q: back | s: unified`);
}

/**
 * Toggle between unified and side-by-side view
 */
globalThis.diff_toggle_view = async function(): Promise<void> {
  if (!diffState.currentFile) {
    editor.setStatus("No file diff open");
    return;
  }

  const useSideBySide = diffState.currentViewMode !== "side-by-side";
  await showFileDiff(diffState.currentFile, useSideBySide);
};

/**
 * Close diff view
 */
globalThis.diff_close = function(): void {
  if (!diffState.isOpen) {
    return;
  }

  if (diffState.currentFile) {
    // Close side-by-side buffers if open
    if (diffState.leftBufferId !== null) {
      editor.closeBuffer(diffState.leftBufferId);
      diffState.leftBufferId = null;
      diffState.leftSplitId = null;
    }
    if (diffState.rightBufferId !== null) {
      editor.closeBuffer(diffState.rightBufferId);
      diffState.rightBufferId = null;
      diffState.rightSplitId = null;
    }

    // Close unified buffer if open
    if (diffState.bufferId !== null) {
      editor.closeBuffer(diffState.bufferId);
      diffState.bufferId = null;
    }

    // Go back to file list
    diffState.currentFile = null;
    diffState.currentViewMode = "list";

    // Recreate file list
    if (diffState.splitId !== null) {
      const entries = buildFileListEntries(diffState.files);
      editor.createVirtualBufferInExistingSplit({
        name: "*Diff*",
        mode: MODE_DIFF_LIST,
        read_only: true,
        entries: entries,
        split_id: diffState.splitId,
        show_line_numbers: false,
        show_cursors: true,
        editing_disabled: true,
      }).then((bufferId) => {
        if (bufferId !== null) {
          diffState.bufferId = bufferId;
          editor.setStatus(`Diff: ${diffState.files.length} files | Enter: view diff | q: quit`);
        }
      });
    }
  } else {
    // Close completely
    if (diffState.bufferId !== null) {
      editor.closeBuffer(diffState.bufferId);
    }
    if (diffState.leftBufferId !== null) {
      editor.closeBuffer(diffState.leftBufferId);
    }
    if (diffState.rightBufferId !== null) {
      editor.closeBuffer(diffState.rightBufferId);
    }

    diffState.isOpen = false;
    diffState.bufferId = null;
    diffState.leftBufferId = null;
    diffState.rightBufferId = null;
    diffState.splitId = null;
    diffState.leftSplitId = null;
    diffState.rightSplitId = null;
    diffState.sourceBufferId = null;
    diffState.files = [];
    diffState.currentFile = null;
    diffState.currentViewMode = "list";
  }
};

// =============================================================================
// Mode Definition
// =============================================================================

// Define diff-list mode
editor.defineMode(MODE_DIFF_LIST, "special", [
  ["Return", "diff_view_file"],
  ["q", "diff_close"],
], true);

// Define diff-view mode
editor.defineMode(MODE_DIFF_VIEW, "special", [
  ["q", "diff_close"],
  ["s", "diff_toggle_view"],
], true);

// =============================================================================
// Command Handlers
// =============================================================================

/**
 * Handle Enter key in diff list to view file diff
 */
globalThis.diff_view_file = async function(): Promise<void> {
  if (!diffState.isOpen) {
    return;
  }

  // If we're in side-by-side view, toggle back to unified first
  if (diffState.currentViewMode === "side-by-side") {
    if (diffState.leftBufferId !== null) {
      editor.closeBuffer(diffState.leftBufferId);
      diffState.leftBufferId = null;
    }
    if (diffState.rightBufferId !== null) {
      editor.closeBuffer(diffState.rightBufferId);
      diffState.rightBufferId = null;
    }
  }

  const bufferId = diffState.bufferId;
  if (bufferId === null) {
    return;
  }

  const cursorPos = editor.getCursorPosition();
  
  // Get text properties at cursor to find which file was selected
  const props = editor.getTextPropertiesAtCursor(bufferId);
  if (props && props.file) {
    await showFileDiff(props.file as string, false); // Start with unified view
  } else {
    editor.setStatus("No file selected");
  }
};

// =============================================================================
// Prompt Handlers
// =============================================================================

/**
 * Handle directory diff prompts
 */
globalThis.onDiffDirectoryPromptConfirmed = async function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  if (args.prompt_type !== "diff-directory") {
    return true;
  }

  const input = args.input.trim();

  if (promptState.type === "directory-left") {
    if (!input) {
      editor.setStatus("Directory diff cancelled");
      promptState.type = null;
      return true;
    }

    promptState.leftValue = input;
    promptState.type = "directory-right";
    editor.startPrompt("Right directory:", "diff-directory");
    editor.setStatus("Enter right directory path");
  } else if (promptState.type === "directory-right") {
    if (!input) {
      editor.setStatus("Directory diff cancelled");
      promptState.type = null;
      return true;
    }

    promptState.rightValue = input;
    promptState.type = null;

    editor.setStatus("Comparing directories...");

    diffState.diffType = "directory";
    diffState.leftPath = promptState.leftValue;
    diffState.rightPath = promptState.rightValue;

    const files = await compareDirectories(promptState.leftValue, promptState.rightValue);
    diffState.files = files;

    await showDiffList();
  }

  return true;
};

/**
 * Handle git diff prompts
 */
globalThis.onDiffGitPromptConfirmed = async function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  if (args.prompt_type !== "diff-git") {
    return true;
  }

  const bufferId = editor.getActiveBufferId();
  const filePath = editor.getBufferPath(bufferId);
  if (!filePath) {
    editor.setStatus("No file open");
    promptState.type = null;
    return true;
  }

  const cwd = editor.pathDirname(filePath);
  const gitRoot = await getGitRoot(cwd);
  if (!gitRoot) {
    editor.setStatus("Not in a git repository");
    promptState.type = null;
    return true;
  }

  const right = args.input.trim() || "HEAD";
  const left = ""; // Working directory

  editor.setStatus("Loading git diff...");

  diffState.diffType = "git";
  diffState.leftPath = left || "working directory";
  diffState.rightPath = right;

  const files = await getGitDiffFiles(left || "HEAD", right, gitRoot);
  
  // Get stats for each file
  for (const file of files) {
    const stats = await getGitDiffStats(left || "HEAD", right, file.path, gitRoot);
    file.additions = stats.additions;
    file.deletions = stats.deletions;
  }

  diffState.files = files;
  promptState.type = null;

  await showDiffList();

  return true;
};

/**
 * Handle commit diff prompts
 */
globalThis.onDiffCommitPromptConfirmed = async function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  if (args.prompt_type !== "diff-commit") {
    return true;
  }

  const bufferId = editor.getActiveBufferId();
  const filePath = editor.getBufferPath(bufferId);
  if (!filePath) {
    editor.setStatus("No file open");
    promptState.type = null;
    return true;
  }

  const cwd = editor.pathDirname(filePath);
  const gitRoot = await getGitRoot(cwd);
  if (!gitRoot) {
    editor.setStatus("Not in a git repository");
    promptState.type = null;
    return true;
  }

  const input = args.input.trim();

  if (promptState.type === "commit-left") {
    if (!input) {
      editor.setStatus("Commit diff cancelled");
      promptState.type = null;
      return true;
    }

    promptState.leftValue = input;
    promptState.type = "commit-right";
    editor.startPrompt("Right commit/branch/tag:", "diff-commit");
    editor.setStatus("Enter right git ref");
  } else if (promptState.type === "commit-right") {
    if (!input) {
      editor.setStatus("Commit diff cancelled");
      promptState.type = null;
      return true;
    }

    promptState.rightValue = input;
    promptState.type = null;

    editor.setStatus("Loading commit diff...");

    diffState.diffType = "commit";
    diffState.leftPath = promptState.leftValue;
    diffState.rightPath = promptState.rightValue;

    const files = await getGitDiffFiles(promptState.leftValue, promptState.rightValue, gitRoot);
    
    // Get stats for each file
    for (const file of files) {
      const stats = await getGitDiffStats(promptState.leftValue, promptState.rightValue, file.path, gitRoot);
      file.additions = stats.additions;
      file.deletions = stats.deletions;
    }

    diffState.files = files;

    await showDiffList();
  }

  return true;
};

/**
 * Handle prompt cancellation
 */
globalThis.onDiffPromptCancelled = function(args: {
  prompt_type: string;
}): boolean {
  if (args.prompt_type !== "diff-directory" &&
      args.prompt_type !== "diff-git" &&
      args.prompt_type !== "diff-commit") {
    return true;
  }

  editor.setStatus("Diff cancelled");
  promptState.type = null;
  promptState.leftValue = "";
  promptState.rightValue = "";

  return true;
};

// Register event handlers
editor.on("prompt_confirmed", "onDiffDirectoryPromptConfirmed");
editor.on("prompt_confirmed", "onDiffGitPromptConfirmed");
editor.on("prompt_confirmed", "onDiffCommitPromptConfirmed");
editor.on("prompt_cancelled", "onDiffPromptCancelled");

// =============================================================================
// Command Registration
// =============================================================================

editor.registerCommand(
  "Show Directory Diff",
  "Compare two directories",
  "show_directory_diff"
);

editor.registerCommand(
  "Show Git Diff",
  "Show git diff (local vs branch/commit/remote)",
  "show_git_diff"
);

editor.registerCommand(
  "Show Commit Diff",
  "Compare two commits/branches/tags",
  "show_commit_diff"
);

