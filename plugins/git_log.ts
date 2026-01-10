/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Git Log Plugin - Magit-style Git Log Interface
 *
 * Provides an interactive git log view with:
 * - Syntax highlighting for hash, author, date, subject
 * - Cursor navigation between commits
 * - Enter to open commit details in a virtual buffer
 *
 * Architecture designed for future magit-style features.
 */

// =============================================================================
// Types and Interfaces
// =============================================================================

interface GitCommit {
  hash: string;
  shortHash: string;
  author: string;
  authorEmail: string;
  date: string;
  relativeDate: string;
  subject: string;
  body: string;
  refs: string;      // Branch/tag refs
  graph: string;     // Graph characters
}

interface GitLogOptions {
  showGraph: boolean;
  showRefs: boolean;
  maxCommits: number;
}

interface GitLogState {
  isOpen: boolean;
  bufferId: number | null;
  splitId: number | null; // The split where git log is displayed
  sourceBufferId: number | null; // The buffer that was open before git log (to restore on close)
  commits: GitCommit[];
  options: GitLogOptions;
  cachedContent: string; // Store content for highlighting (getBufferText doesn't work for virtual buffers)
}

interface GitCommitDetailState {
  isOpen: boolean;
  bufferId: number | null;
  splitId: number | null;
  commit: GitCommit | null;
  cachedContent: string; // Store content for highlighting
}

interface GitFileViewState {
  isOpen: boolean;
  bufferId: number | null;
  splitId: number | null;
  filePath: string | null;
  commitHash: string | null;
}

// =============================================================================
// State Management
// =============================================================================

const gitLogState: GitLogState = {
  isOpen: false,
  bufferId: null,
  splitId: null,
  sourceBufferId: null,
  commits: [],
  options: {
    showGraph: false,  // Disabled by default - graph interferes with format parsing
    showRefs: true,
    maxCommits: 100,
  },
  cachedContent: "",
};

const commitDetailState: GitCommitDetailState = {
  isOpen: false,
  bufferId: null,
  splitId: null,
  commit: null,
  cachedContent: "",
};

const fileViewState: GitFileViewState = {
  isOpen: false,
  bufferId: null,
  splitId: null,
  filePath: null,
  commitHash: null,
};

// =============================================================================
// Color Definitions (for syntax highlighting)
// =============================================================================

const colors = {
  hash: [255, 180, 50] as [number, number, number],       // Yellow/Orange
  author: [100, 200, 255] as [number, number, number],    // Cyan
  date: [150, 255, 150] as [number, number, number],      // Green
  subject: [255, 255, 255] as [number, number, number],   // White
  header: [255, 200, 100] as [number, number, number],    // Gold
  separator: [100, 100, 100] as [number, number, number], // Gray
  selected: [80, 80, 120] as [number, number, number],    // Selection background
  diffAdd: [100, 255, 100] as [number, number, number],   // Green for additions
  diffDel: [255, 100, 100] as [number, number, number],   // Red for deletions
  diffHunk: [150, 150, 255] as [number, number, number],  // Blue for hunk headers
  branch: [255, 150, 255] as [number, number, number],    // Magenta for branches
  tag: [255, 255, 100] as [number, number, number],       // Yellow for tags
  remote: [255, 130, 100] as [number, number, number],    // Orange for remotes
  graph: [150, 150, 150] as [number, number, number],     // Gray for graph
  // Syntax highlighting colors
  syntaxKeyword: [200, 120, 220] as [number, number, number],  // Purple for keywords
  syntaxString: [180, 220, 140] as [number, number, number],   // Light green for strings
  syntaxComment: [120, 120, 120] as [number, number, number],  // Gray for comments
  syntaxNumber: [220, 180, 120] as [number, number, number],   // Orange for numbers
  syntaxFunction: [100, 180, 255] as [number, number, number], // Blue for functions
  syntaxType: [80, 200, 180] as [number, number, number],      // Teal for types
};

// =============================================================================
// Mode Definitions
// =============================================================================

// Define git-log mode with minimal keybindings
// Navigation uses normal cursor movement (arrows, j/k work naturally via parent mode)
editor.defineMode(
  "git-log",
  "normal", // inherit from normal mode for cursor movement
  [
    ["Return", "git_log_show_commit"],
    ["Tab", "git_log_show_commit"],
    ["q", "git_log_close"],
    ["Escape", "git_log_close"],
    ["r", "git_log_refresh"],
    ["y", "git_log_copy_hash"],
  ],
  true // read-only
);

// Define git-commit-detail mode for viewing commit details
// Inherits from normal mode for natural cursor movement
editor.defineMode(
  "git-commit-detail",
  "normal", // inherit from normal mode for cursor movement
  [
    ["Return", "git_commit_detail_open_file"],
    ["q", "git_commit_detail_close"],
    ["Escape", "git_commit_detail_close"],
  ],
  true // read-only
);

// Define git-file-view mode for viewing files at a specific commit
editor.defineMode(
  "git-file-view",
  "normal", // inherit from normal mode for cursor movement
  [
    ["q", "git_file_view_close"],
    ["Escape", "git_file_view_close"],
  ],
  true // read-only
);

// =============================================================================
// Git Command Execution
// =============================================================================

async function fetchGitLog(): Promise<GitCommit[]> {
  // Use record separator to reliably split commits
  // Format: hash, short hash, author, email, date, relative date, refs, subject, body
  const format = "%H%x00%h%x00%an%x00%ae%x00%ai%x00%ar%x00%d%x00%s%x00%b%x1e";

  const args = [
    "log",
    `--format=${format}`,
    `-n${gitLogState.options.maxCommits}`,
  ];

  const cwd = editor.getCwd();
  const result = await editor.spawnProcess("git", args, cwd);

  if (result.exit_code !== 0) {
    editor.setStatus(editor.t("status.git_error", { error: result.stderr }));
    return [];
  }

  const commits: GitCommit[] = [];
  // Split by record separator (0x1e)
  const records = result.stdout.split("\x1e");

  for (const record of records) {
    if (!record.trim()) continue;

    const parts = record.split("\x00");
    if (parts.length >= 8) {
      commits.push({
        hash: parts[0].trim(),
        shortHash: parts[1].trim(),
        author: parts[2].trim(),
        authorEmail: parts[3].trim(),
        date: parts[4].trim(),
        relativeDate: parts[5].trim(),
        refs: parts[6].trim(),
        subject: parts[7].trim(),
        body: parts[8] ? parts[8].trim() : "",
        graph: "", // Graph is handled separately if needed
      });
    }
  }

  return commits;
}

async function fetchCommitDiff(hash: string): Promise<string> {
  const cwd = editor.getCwd();
  const result = await editor.spawnProcess("git", [
    "show",
    "--stat",
    "--patch",
    hash,
  ], cwd);

  if (result.exit_code !== 0) {
    return editor.t("status.error_fetching_diff", { error: result.stderr });
  }

  return result.stdout;
}

// =============================================================================
// Git Log View
// =============================================================================

function formatCommitRow(commit: GitCommit): string {
  // Build a structured line for consistent parsing and highlighting
  // Format: shortHash (author, relativeDate) subject [refs]
  let line = commit.shortHash;

  // Add author in parentheses
  line += " (" + commit.author + ", " + commit.relativeDate + ")";

  // Add subject
  line += " " + commit.subject;

  // Add refs at the end if present and enabled
  if (gitLogState.options.showRefs && commit.refs) {
    line += " " + commit.refs;
  }

  return line + "\n";
}

// Helper to extract content string from entries (for highlighting)
function entriesToContent(entries: TextPropertyEntry[]): string {
  return entries.map(e => e.text).join("");
}

function buildGitLogEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Magit-style header
  entries.push({
    text: editor.t("panel.commits_header") + "\n",
    properties: { type: "section-header" },
  });

  if (gitLogState.commits.length === 0) {
    entries.push({
      text: editor.t("panel.no_commits") + "\n",
      properties: { type: "empty" },
    });
  } else {
    // Add each commit
    for (let i = 0; i < gitLogState.commits.length; i++) {
      const commit = gitLogState.commits[i];
      entries.push({
        text: formatCommitRow(commit),
        properties: {
          type: "commit",
          index: i,
          hash: commit.hash,
          shortHash: commit.shortHash,
          author: commit.author,
          date: commit.relativeDate,
          subject: commit.subject,
          refs: commit.refs,
          graph: commit.graph,
        },
      });
    }
  }

  // Footer with help
  entries.push({
    text: "\n",
    properties: { type: "blank" },
  });
  entries.push({
    text: editor.t("panel.log_footer", { count: String(gitLogState.commits.length) }) + "\n",
    properties: { type: "footer" },
  });

  return entries;
}

function applyGitLogHighlighting(): void {
  if (gitLogState.bufferId === null) return;

  const bufferId = gitLogState.bufferId;

  // Clear existing overlays
  editor.clearNamespace(bufferId, "gitlog");

  // Use cached content (getBufferText doesn't work for virtual buffers)
  const content = gitLogState.cachedContent;
  if (!content) return;
  const lines = content.split("\n");

  // Get cursor line to highlight current row (1-indexed from API)
  const cursorLine = editor.getCursorLine();
  const headerLines = 1; // Just "Commits:" header

  let byteOffset = 0;

  for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
    const line = lines[lineIdx];
    const lineStart = byteOffset;
    const lineEnd = byteOffset + line.length;

    // Highlight section header
    if (line === editor.t("panel.commits_header")) {
      editor.addOverlay(
        bufferId,
        "gitlog",
        lineStart,
        lineEnd,
        colors.header[0],
        colors.header[1],
        colors.header[2],
        true,  // underline
        true,  // bold
        false  // italic
      );
      byteOffset += line.length + 1;
      continue;
    }

    const commitIndex = lineIdx - headerLines;
    if (commitIndex < 0 || commitIndex >= gitLogState.commits.length) {
      byteOffset += line.length + 1;
      continue;
    }

    const commit = gitLogState.commits[commitIndex];
    // cursorLine is 1-indexed, lineIdx is 0-indexed
    const isCurrentLine = (lineIdx + 1) === cursorLine;

    // Highlight entire line if cursor is on it (using selected color with underline)
    if (isCurrentLine) {
      editor.addOverlay(
        bufferId,
        "gitlog",
        lineStart,
        lineEnd,
        colors.selected[0],
        colors.selected[1],
        colors.selected[2],
        true,  // underline to make it visible
        true,  // bold
        false  // italic
      );
    }

    // Parse the line format: "shortHash (author, relativeDate) subject [refs]"
    // Highlight hash (first 7+ chars until space)
    const hashEnd = commit.shortHash.length;
    editor.addOverlay(
      bufferId,
      "gitlog",
      lineStart,
      lineStart + hashEnd,
      colors.hash[0],
      colors.hash[1],
      colors.hash[2],
      false, // underline
      false, // bold
      false  // italic
    );

    // Highlight author name (inside parentheses)
    const authorPattern = "(" + commit.author + ",";
    const authorStartInLine = line.indexOf(authorPattern);
    if (authorStartInLine >= 0) {
      const authorStart = lineStart + authorStartInLine + 1; // skip "("
      const authorEnd = authorStart + commit.author.length;
      editor.addOverlay(
        bufferId,
        "gitlog",
        authorStart,
        authorEnd,
        colors.author[0],
        colors.author[1],
        colors.author[2],
        false, // underline
        false, // bold
        false  // italic
      );
    }

    // Highlight relative date
    const datePattern = ", " + commit.relativeDate + ")";
    const dateStartInLine = line.indexOf(datePattern);
    if (dateStartInLine >= 0) {
      const dateStart = lineStart + dateStartInLine + 2; // skip ", "
      const dateEnd = dateStart + commit.relativeDate.length;
      editor.addOverlay(
        bufferId,
        "gitlog",
        dateStart,
        dateEnd,
        colors.date[0],
        colors.date[1],
        colors.date[2],
        false, // underline
        false, // bold
        false  // italic
      );
    }

    // Highlight refs (branches/tags) at end of line if present
    if (gitLogState.options.showRefs && commit.refs) {
      const refsStartInLine = line.lastIndexOf(commit.refs);
      if (refsStartInLine >= 0) {
        const refsStart = lineStart + refsStartInLine;
        const refsEnd = refsStart + commit.refs.length;

        // Determine color based on ref type
        let refColor = colors.branch;
        if (commit.refs.includes("tag:")) {
          refColor = colors.tag;
        } else if (commit.refs.includes("origin/") || commit.refs.includes("remote")) {
          refColor = colors.remote;
        }

        editor.addOverlay(
          bufferId,
          "gitlog",
          refsStart,
          refsEnd,
          refColor[0],
          refColor[1],
          refColor[2],
          false, // underline
          true,  // bold (make refs stand out)
          false  // italic
        );
      }
    }

    byteOffset += line.length + 1;
  }
}

function updateGitLogView(): void {
  if (gitLogState.bufferId !== null) {
    const entries = buildGitLogEntries();
    gitLogState.cachedContent = entriesToContent(entries);
    editor.setVirtualBufferContent(gitLogState.bufferId, entries);
    applyGitLogHighlighting();
  }
}

// =============================================================================
// Commit Detail View
// =============================================================================

// Parse diff line to extract file and line information
interface DiffContext {
  currentFile: string | null;
  currentHunkNewStart: number;
  currentHunkNewLine: number;  // Current line within the new file
}

function buildCommitDetailEntries(commit: GitCommit, showOutput: string): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];
  const lines = showOutput.split("\n");

  // Track diff context for file/line navigation
  const diffContext: DiffContext = {
    currentFile: null,
    currentHunkNewStart: 0,
    currentHunkNewLine: 0,
  };

  for (const line of lines) {
    let lineType = "text";
    const properties: Record<string, unknown> = { type: lineType };

    // Detect diff file header: diff --git a/path b/path
    const diffHeaderMatch = line.match(/^diff --git a\/(.+) b\/(.+)$/);
    if (diffHeaderMatch) {
      diffContext.currentFile = diffHeaderMatch[2]; // Use the 'b' (new) file path
      diffContext.currentHunkNewStart = 0;
      diffContext.currentHunkNewLine = 0;
      lineType = "diff-header";
      properties.type = lineType;
      properties.file = diffContext.currentFile;
    }
    // Detect +++ line (new file path)
    else if (line.startsWith("+++ b/")) {
      diffContext.currentFile = line.slice(6);
      lineType = "diff-header";
      properties.type = lineType;
      properties.file = diffContext.currentFile;
    }
    // Detect hunk header: @@ -old,count +new,count @@
    else if (line.startsWith("@@")) {
      lineType = "diff-hunk";
      const hunkMatch = line.match(/@@ -\d+(?:,\d+)? \+(\d+)(?:,\d+)? @@/);
      if (hunkMatch) {
        diffContext.currentHunkNewStart = parseInt(hunkMatch[1], 10);
        diffContext.currentHunkNewLine = diffContext.currentHunkNewStart;
      }
      properties.type = lineType;
      properties.file = diffContext.currentFile;
      properties.line = diffContext.currentHunkNewStart;
    }
    // Addition line
    else if (line.startsWith("+") && !line.startsWith("+++")) {
      lineType = "diff-add";
      properties.type = lineType;
      properties.file = diffContext.currentFile;
      properties.line = diffContext.currentHunkNewLine;
      diffContext.currentHunkNewLine++;
    }
    // Deletion line
    else if (line.startsWith("-") && !line.startsWith("---")) {
      lineType = "diff-del";
      properties.type = lineType;
      properties.file = diffContext.currentFile;
      // Deletion lines don't advance the new file line counter
    }
    // Context line (unchanged)
    else if (line.startsWith(" ") && diffContext.currentFile && diffContext.currentHunkNewLine > 0) {
      lineType = "diff-context";
      properties.type = lineType;
      properties.file = diffContext.currentFile;
      properties.line = diffContext.currentHunkNewLine;
      diffContext.currentHunkNewLine++;
    }
    // Other diff header lines
    else if (line.startsWith("index ") || line.startsWith("--- ")) {
      lineType = "diff-header";
      properties.type = lineType;
    }
    // Commit header lines
    else if (line.startsWith("commit ")) {
      lineType = "header";
      properties.type = lineType;
      const hashMatch = line.match(/^commit ([a-f0-9]+)/);
      if (hashMatch) {
        properties.hash = hashMatch[1];
      }
    }
    else if (line.startsWith("Author:")) {
      lineType = "meta";
      properties.type = lineType;
      properties.field = "author";
    }
    else if (line.startsWith("Date:")) {
      lineType = "meta";
      properties.type = lineType;
      properties.field = "date";
    }

    entries.push({
      text: `${line}\n`,
      properties: properties,
    });
  }

  // Footer with help
  entries.push({
    text: "\n",
    properties: { type: "blank" },
  });
  entries.push({
    text: editor.t("panel.detail_footer") + "\n",
    properties: { type: "footer" },
  });

  return entries;
}

function applyCommitDetailHighlighting(): void {
  if (commitDetailState.bufferId === null) return;

  const bufferId = commitDetailState.bufferId;

  // Clear existing overlays
  editor.clearNamespace(bufferId, "gitdetail");

  // Use cached content (getBufferText doesn't work for virtual buffers)
  const content = commitDetailState.cachedContent;
  if (!content) return;
  const lines = content.split("\n");

  let byteOffset = 0;

  for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
    const line = lines[lineIdx];
    const lineStart = byteOffset;
    const lineEnd = byteOffset + line.length;

    // Highlight diff additions (green)
    if (line.startsWith("+") && !line.startsWith("+++")) {
      editor.addOverlay(
        bufferId,
        "gitdetail",
        lineStart,
        lineEnd,
        colors.diffAdd[0],
        colors.diffAdd[1],
        colors.diffAdd[2],
        false, // underline
        false, // bold
        false  // italic
      );
    }
    // Highlight diff deletions (red)
    else if (line.startsWith("-") && !line.startsWith("---")) {
      editor.addOverlay(
        bufferId,
        "gitdetail",
        lineStart,
        lineEnd,
        colors.diffDel[0],
        colors.diffDel[1],
        colors.diffDel[2],
        false, // underline
        false, // bold
        false  // italic
      );
    }
    // Highlight hunk headers (cyan/blue)
    else if (line.startsWith("@@")) {
      editor.addOverlay(
        bufferId,
        "gitdetail",
        lineStart,
        lineEnd,
        colors.diffHunk[0],
        colors.diffHunk[1],
        colors.diffHunk[2],
        false, // underline
        true,  // bold
        false  // italic
      );
    }
    // Highlight commit hash in "commit <hash>" line (git show format)
    else if (line.startsWith("commit ")) {
      const hashMatch = line.match(/^commit ([a-f0-9]+)/);
      if (hashMatch) {
        const hashStart = lineStart + 7; // "commit " is 7 chars
        editor.addOverlay(
          bufferId,
          "gitdetail",
          hashStart,
          hashStart + hashMatch[1].length,
          colors.hash[0],
          colors.hash[1],
          colors.hash[2],
          false, // underline
          true,  // bold
          false  // italic
        );
      }
    }
    // Highlight author line
    else if (line.startsWith("Author:")) {
      editor.addOverlay(
        bufferId,
        "gitdetail",
        lineStart + 8, // "Author: " is 8 chars
        lineEnd,
        colors.author[0],
        colors.author[1],
        colors.author[2],
        false, // underline
        false, // bold
        false  // italic
      );
    }
    // Highlight date line
    else if (line.startsWith("Date:")) {
      editor.addOverlay(
        bufferId,
        "gitdetail",
        lineStart + 6, // "Date: " is 6 chars (with trailing spaces it's 8)
        lineEnd,
        colors.date[0],
        colors.date[1],
        colors.date[2],
        false, // underline
        false, // bold
        false  // italic
      );
    }
    // Highlight diff file headers
    else if (line.startsWith("diff --git")) {
      editor.addOverlay(
        bufferId,
        "gitdetail",
        lineStart,
        lineEnd,
        colors.header[0],
        colors.header[1],
        colors.header[2],
        false, // underline
        true,  // bold
        false  // italic
      );
    }

    byteOffset += line.length + 1;
  }
}

// =============================================================================
// Public Commands - Git Log
// =============================================================================

globalThis.show_git_log = async function(): Promise<void> {
  if (gitLogState.isOpen) {
    editor.setStatus(editor.t("status.already_open"));
    return;
  }

  editor.setStatus(editor.t("status.loading"));

  // Store the current split ID and buffer ID before opening git log
  gitLogState.splitId = editor.getActiveSplitId();
  gitLogState.sourceBufferId = editor.getActiveBufferId();

  // Fetch commits
  gitLogState.commits = await fetchGitLog();

  if (gitLogState.commits.length === 0) {
    editor.setStatus(editor.t("status.no_commits"));
    gitLogState.splitId = null;
    return;
  }

  // Build entries and cache content for highlighting
  const entries = buildGitLogEntries();
  gitLogState.cachedContent = entriesToContent(entries);

  // Create virtual buffer in the current split (replacing current buffer)
  const bufferId = await editor.createVirtualBufferInExistingSplit({
    name: "*Git Log*",
    mode: "git-log",
    read_only: true,
    entries: entries,
    split_id: gitLogState.splitId!,
    show_line_numbers: false,
    show_cursors: true,
    editing_disabled: true,
  });

  if (bufferId !== null) {
    gitLogState.isOpen = true;
    gitLogState.bufferId = bufferId;

    // Apply syntax highlighting
    applyGitLogHighlighting();

    editor.setStatus(editor.t("status.log_ready", { count: String(gitLogState.commits.length) }));
    editor.debug("Git log panel opened");
  } else {
    gitLogState.splitId = null;
    editor.setStatus(editor.t("status.failed_open"));
  }
};

globalThis.git_log_close = function(): void {
  if (!gitLogState.isOpen) {
    return;
  }

  // Restore the original buffer in the split
  if (gitLogState.splitId !== null && gitLogState.sourceBufferId !== null) {
    editor.setSplitBuffer(gitLogState.splitId, gitLogState.sourceBufferId);
  }

  // Close the git log buffer (it's no longer displayed)
  if (gitLogState.bufferId !== null) {
    editor.closeBuffer(gitLogState.bufferId);
  }

  gitLogState.isOpen = false;
  gitLogState.bufferId = null;
  gitLogState.splitId = null;
  gitLogState.sourceBufferId = null;
  gitLogState.commits = [];
  editor.setStatus(editor.t("status.closed"));
};

// Cursor moved handler for git log - update highlighting and status
globalThis.on_git_log_cursor_moved = function(data: {
  buffer_id: number;
  cursor_id: number;
  old_position: number;
  new_position: number;
}): void {
  // Only handle cursor movement in our git log buffer
  if (gitLogState.bufferId === null || data.buffer_id !== gitLogState.bufferId) {
    return;
  }

  // Re-apply highlighting to update cursor line highlight
  applyGitLogHighlighting();

  // Get cursor line to show status
  const cursorLine = editor.getCursorLine();
  const headerLines = 1;
  const commitIndex = cursorLine - headerLines;

  if (commitIndex >= 0 && commitIndex < gitLogState.commits.length) {
    editor.setStatus(editor.t("status.commit_position", { current: String(commitIndex + 1), total: String(gitLogState.commits.length) }));
  }
};

// Register cursor movement handler
editor.on("cursor_moved", "on_git_log_cursor_moved");

globalThis.git_log_refresh = async function(): Promise<void> {
  if (!gitLogState.isOpen) return;

  editor.setStatus(editor.t("status.refreshing"));
  gitLogState.commits = await fetchGitLog();
  updateGitLogView();
  editor.setStatus(editor.t("status.refreshed", { count: String(gitLogState.commits.length) }));
};

// Helper function to get commit at current cursor position
function getCommitAtCursor(): GitCommit | null {
  if (gitLogState.bufferId === null) return null;

  // Use text properties to find which commit the cursor is on
  // This is more reliable than line number calculation
  const props = editor.getTextPropertiesAtCursor(gitLogState.bufferId);

  if (props.length > 0) {
    const prop = props[0];
    // Check if cursor is on a commit line (has type "commit" and index)
    if (prop.type === "commit" && typeof prop.index === "number") {
      const index = prop.index as number;
      if (index >= 0 && index < gitLogState.commits.length) {
        return gitLogState.commits[index];
      }
    }
    // Also support finding commit by hash (alternative lookup)
    if (prop.hash && typeof prop.hash === "string") {
      return gitLogState.commits.find(c => c.hash === prop.hash) || null;
    }
  }

  return null;
}

globalThis.git_log_show_commit = async function(): Promise<void> {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;
  if (gitLogState.splitId === null) return;

  const commit = getCommitAtCursor();
  if (!commit) {
    editor.setStatus(editor.t("status.move_to_commit"));
    return;
  }

  editor.setStatus(editor.t("status.loading_commit", { hash: commit.shortHash }));

  // Fetch full commit info using git show (includes header and diff)
  const showOutput = await fetchCommitDiff(commit.hash);

  // Build entries using raw git show output
  const entries = buildCommitDetailEntries(commit, showOutput);

  // Cache content for highlighting (getBufferText doesn't work for virtual buffers)
  commitDetailState.cachedContent = entriesToContent(entries);

  // Create virtual buffer in the current split (replacing git log view)
  const bufferId = await editor.createVirtualBufferInExistingSplit({
    name: `*Commit: ${commit.shortHash}*`,
    mode: "git-commit-detail",
    read_only: true,
    entries: entries,
    split_id: gitLogState.splitId!,
    show_line_numbers: false, // Disable line numbers for cleaner diff view
    show_cursors: true,
    editing_disabled: true,
  });

  if (bufferId !== null) {
    commitDetailState.isOpen = true;
    commitDetailState.bufferId = bufferId;
    commitDetailState.splitId = gitLogState.splitId;
    commitDetailState.commit = commit;

    // Apply syntax highlighting
    applyCommitDetailHighlighting();

    editor.setStatus(editor.t("status.commit_ready", { hash: commit.shortHash }));
  } else {
    editor.setStatus(editor.t("status.failed_open_details"));
  }
};

globalThis.git_log_copy_hash = function(): void {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;

  const commit = getCommitAtCursor();
  if (!commit) {
    editor.setStatus(editor.t("status.move_to_commit"));
    return;
  }

  // Copy hash to clipboard
  editor.copyToClipboard(commit.hash);
  editor.setStatus(editor.t("status.hash_copied", { short: commit.shortHash, full: commit.hash }));
};

// =============================================================================
// Public Commands - Commit Detail
// =============================================================================

globalThis.git_commit_detail_close = function(): void {
  if (!commitDetailState.isOpen) {
    return;
  }

  // Go back to the git log view by restoring the git log buffer
  if (commitDetailState.splitId !== null && gitLogState.bufferId !== null) {
    editor.setSplitBuffer(commitDetailState.splitId, gitLogState.bufferId);
    // Re-apply highlighting since we're switching back
    applyGitLogHighlighting();
  }

  // Close the commit detail buffer (it's no longer displayed)
  if (commitDetailState.bufferId !== null) {
    editor.closeBuffer(commitDetailState.bufferId);
  }

  commitDetailState.isOpen = false;
  commitDetailState.bufferId = null;
  commitDetailState.splitId = null;
  commitDetailState.commit = null;

  editor.setStatus(editor.t("status.log_ready", { count: String(gitLogState.commits.length) }));
};

// Close file view and go back to commit detail
globalThis.git_file_view_close = function(): void {
  if (!fileViewState.isOpen) {
    return;
  }

  // Go back to the commit detail view by restoring the commit detail buffer
  if (fileViewState.splitId !== null && commitDetailState.bufferId !== null) {
    editor.setSplitBuffer(fileViewState.splitId, commitDetailState.bufferId);
    // Re-apply highlighting since we're switching back
    applyCommitDetailHighlighting();
  }

  // Close the file view buffer (it's no longer displayed)
  if (fileViewState.bufferId !== null) {
    editor.closeBuffer(fileViewState.bufferId);
  }

  fileViewState.isOpen = false;
  fileViewState.bufferId = null;
  fileViewState.splitId = null;
  fileViewState.filePath = null;
  fileViewState.commitHash = null;

  if (commitDetailState.commit) {
    editor.setStatus(editor.t("status.commit_ready", { hash: commitDetailState.commit.shortHash }));
  }
};

// Fetch file content at a specific commit
async function fetchFileAtCommit(commitHash: string, filePath: string): Promise<string | null> {
  const cwd = editor.getCwd();
  const result = await editor.spawnProcess("git", [
    "show",
    `${commitHash}:${filePath}`,
  ], cwd);

  if (result.exit_code !== 0) {
    return null;
  }

  return result.stdout;
}

// Get language type from file extension
function getLanguageFromPath(filePath: string): string {
  const ext = editor.pathExtname(filePath).toLowerCase();
  const extMap: Record<string, string> = {
    ".rs": "rust",
    ".ts": "typescript",
    ".tsx": "typescript",
    ".js": "javascript",
    ".jsx": "javascript",
    ".py": "python",
    ".go": "go",
    ".c": "c",
    ".cpp": "cpp",
    ".h": "c",
    ".hpp": "cpp",
    ".java": "java",
    ".rb": "ruby",
    ".sh": "shell",
    ".bash": "shell",
    ".zsh": "shell",
    ".toml": "toml",
    ".yaml": "yaml",
    ".yml": "yaml",
    ".json": "json",
    ".md": "markdown",
    ".css": "css",
    ".html": "html",
    ".xml": "xml",
  };
  return extMap[ext] || "text";
}

// Keywords for different languages
const languageKeywords: Record<string, string[]> = {
  rust: ["fn", "let", "mut", "const", "pub", "use", "mod", "struct", "enum", "impl", "trait", "for", "while", "loop", "if", "else", "match", "return", "async", "await", "move", "self", "Self", "super", "crate", "where", "type", "static", "unsafe", "extern", "ref", "dyn", "as", "in", "true", "false"],
  typescript: ["function", "const", "let", "var", "class", "interface", "type", "extends", "implements", "import", "export", "from", "async", "await", "return", "if", "else", "for", "while", "do", "switch", "case", "break", "continue", "new", "this", "super", "null", "undefined", "true", "false", "try", "catch", "finally", "throw", "typeof", "instanceof", "void", "delete", "in", "of", "static", "readonly", "private", "public", "protected", "abstract", "enum"],
  javascript: ["function", "const", "let", "var", "class", "extends", "import", "export", "from", "async", "await", "return", "if", "else", "for", "while", "do", "switch", "case", "break", "continue", "new", "this", "super", "null", "undefined", "true", "false", "try", "catch", "finally", "throw", "typeof", "instanceof", "void", "delete", "in", "of", "static"],
  python: ["def", "class", "if", "elif", "else", "for", "while", "try", "except", "finally", "with", "as", "import", "from", "return", "yield", "raise", "pass", "break", "continue", "and", "or", "not", "in", "is", "lambda", "None", "True", "False", "global", "nonlocal", "async", "await", "self"],
  go: ["func", "var", "const", "type", "struct", "interface", "map", "chan", "if", "else", "for", "range", "switch", "case", "default", "break", "continue", "return", "go", "defer", "select", "import", "package", "nil", "true", "false", "make", "new", "len", "cap", "append", "copy", "delete", "panic", "recover"],
};

// Apply basic syntax highlighting to file view
function applyFileViewHighlighting(bufferId: number, content: string, filePath: string): void {
  const language = getLanguageFromPath(filePath);
  const keywords = languageKeywords[language] || [];
  const lines = content.split("\n");

  // Clear existing overlays
  editor.clearNamespace(bufferId, "syntax");

  let byteOffset = 0;
  let inMultilineComment = false;
  let inMultilineString = false;

  for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
    const line = lines[lineIdx];
    const lineStart = byteOffset;

    // Skip empty lines
    if (line.trim() === "") {
      byteOffset += line.length + 1;
      continue;
    }

    // Check for multiline comment start/end
    if (language === "rust" || language === "c" || language === "cpp" || language === "java" || language === "javascript" || language === "typescript" || language === "go") {
      if (line.includes("/*") && !line.includes("*/")) {
        inMultilineComment = true;
      }
      if (inMultilineComment) {
        editor.addOverlay(bufferId, "syntax", lineStart, lineStart + line.length, colors.syntaxComment[0], colors.syntaxComment[1], colors.syntaxComment[2], false, false, true);
        if (line.includes("*/")) {
          inMultilineComment = false;
        }
        byteOffset += line.length + 1;
        continue;
      }
    }

    // Python multiline strings
    if (language === "python" && (line.includes('"""') || line.includes("'''"))) {
      const tripleQuote = line.includes('"""') ? '"""' : "'''";
      const firstIdx = line.indexOf(tripleQuote);
      const secondIdx = line.indexOf(tripleQuote, firstIdx + 3);
      if (firstIdx >= 0 && secondIdx < 0) {
        inMultilineString = !inMultilineString;
      }
    }
    if (inMultilineString) {
      editor.addOverlay(bufferId, "syntax", lineStart, lineStart + line.length, colors.syntaxString[0], colors.syntaxString[1], colors.syntaxString[2], false, false, false);
      byteOffset += line.length + 1;
      continue;
    }

    // Single-line comment detection
    let commentStart = -1;
    if (language === "rust" || language === "c" || language === "cpp" || language === "java" || language === "javascript" || language === "typescript" || language === "go") {
      commentStart = line.indexOf("//");
    } else if (language === "python" || language === "shell" || language === "ruby" || language === "yaml" || language === "toml") {
      commentStart = line.indexOf("#");
    }

    if (commentStart >= 0) {
      editor.addOverlay(bufferId, "syntax", lineStart + commentStart, lineStart + line.length, colors.syntaxComment[0], colors.syntaxComment[1], colors.syntaxComment[2], false, false, true);
    }

    // String highlighting (simple: find "..." and '...')
    let i = 0;
    let stringCount = 0;
    while (i < line.length) {
      const ch = line[i];
      if (ch === '"' || ch === "'") {
        const quote = ch;
        const start = i;
        i++;
        while (i < line.length && line[i] !== quote) {
          if (line[i] === '\\') i++; // Skip escaped chars
          i++;
        }
        if (i < line.length) i++; // Include closing quote
        const end = i;
        if (commentStart < 0 || start < commentStart) {
          editor.addOverlay(bufferId, "syntax", lineStart + start, lineStart + end, colors.syntaxString[0], colors.syntaxString[1], colors.syntaxString[2], false, false, false);
        }
      } else {
        i++;
      }
    }

    // Keyword highlighting
    for (const keyword of keywords) {
      const regex = new RegExp(`\\b${keyword}\\b`, "g");
      let match;
      while ((match = regex.exec(line)) !== null) {
        const kwStart = match.index;
        const kwEnd = kwStart + keyword.length;
        // Don't highlight if inside comment
        if (commentStart < 0 || kwStart < commentStart) {
          editor.addOverlay(bufferId, "syntax", lineStart + kwStart, lineStart + kwEnd, colors.syntaxKeyword[0], colors.syntaxKeyword[1], colors.syntaxKeyword[2], false, true, false);
        }
      }
    }

    // Number highlighting
    const numberRegex = /\b\d+(\.\d+)?\b/g;
    let numMatch;
    while ((numMatch = numberRegex.exec(line)) !== null) {
      const numStart = numMatch.index;
      const numEnd = numStart + numMatch[0].length;
      if (commentStart < 0 || numStart < commentStart) {
        editor.addOverlay(bufferId, "syntax", lineStart + numStart, lineStart + numEnd, colors.syntaxNumber[0], colors.syntaxNumber[1], colors.syntaxNumber[2], false, false, false);
      }
    }

    byteOffset += line.length + 1;
  }
}

// Open file at the current diff line position - shows file as it was at that commit
globalThis.git_commit_detail_open_file = async function(): Promise<void> {
  if (!commitDetailState.isOpen || commitDetailState.bufferId === null) {
    return;
  }

  const commit = commitDetailState.commit;
  if (!commit) {
    editor.setStatus(editor.t("status.move_to_commit"));
    return;
  }

  // Get text properties at cursor position to find file/line info
  const props = editor.getTextPropertiesAtCursor(commitDetailState.bufferId);

  if (props.length > 0) {
    const file = props[0].file as string | undefined;
    const line = props[0].line as number | undefined;

    if (file) {
      editor.setStatus(editor.t("status.file_loading", { file, hash: commit.shortHash }));

      // Fetch file content at this commit
      const content = await fetchFileAtCommit(commit.hash, file);

      if (content === null) {
        editor.setStatus(editor.t("status.file_not_found", { file, hash: commit.shortHash }));
        return;
      }

      // Build entries for the virtual buffer - one entry per line for proper line tracking
      const lines = content.split("\n");
      const entries: TextPropertyEntry[] = [];

      for (let i = 0; i < lines.length; i++) {
        entries.push({
          text: lines[i] + (i < lines.length - 1 ? "\n" : ""),
          properties: { type: "content", line: i + 1 },
        });
      }

      // Create a read-only virtual buffer with the file content
      const bufferId = await editor.createVirtualBufferInExistingSplit({
        name: `${file} @ ${commit.shortHash}`,
        mode: "git-file-view",
        read_only: true,
        entries: entries,
        split_id: commitDetailState.splitId!,
        show_line_numbers: true,
        show_cursors: true,
        editing_disabled: true,
      });

      if (bufferId !== null) {
        // Track file view state so we can navigate back
        fileViewState.isOpen = true;
        fileViewState.bufferId = bufferId;
        fileViewState.splitId = commitDetailState.splitId;
        fileViewState.filePath = file;
        fileViewState.commitHash = commit.hash;

        // Apply syntax highlighting based on file type
        applyFileViewHighlighting(bufferId, content, file);

        const targetLine = line || 1;
        editor.setStatus(editor.t("status.file_view_ready", { file, hash: commit.shortHash, line: String(targetLine) }));
      } else {
        editor.setStatus(editor.t("status.failed_open_file", { file }));
      }
    } else {
      editor.setStatus(editor.t("status.move_to_diff_with_context"));
    }
  } else {
    editor.setStatus(editor.t("status.move_to_diff"));
  }
};

// =============================================================================
// Command Registration
// =============================================================================

editor.registerCommand(
  "%cmd.git_log",
  "%cmd.git_log_desc",
  "show_git_log",
  "normal"
);

editor.registerCommand(
  "%cmd.git_log_close",
  "%cmd.git_log_close_desc",
  "git_log_close",
  "normal"
);

editor.registerCommand(
  "%cmd.git_log_refresh",
  "%cmd.git_log_refresh_desc",
  "git_log_refresh",
  "normal"
);

// =============================================================================
// Plugin Initialization
// =============================================================================

editor.setStatus(editor.t("status.ready", { count: "0" }));
editor.debug("Git Log plugin initialized - Use 'Git Log' command to open");
