/// <reference path="../types/fresh.d.ts" />

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
}

interface GitCommitDetailState {
  isOpen: boolean;
  bufferId: number | null;
  splitId: number | null;
  commit: GitCommit | null;
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
};

const commitDetailState: GitCommitDetailState = {
  isOpen: false,
  bufferId: null,
  splitId: null,
  commit: null,
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

  const result = await editor.spawnProcess("git", args);

  if (result.exit_code !== 0) {
    editor.setStatus(`Git log error: ${result.stderr}`);
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
  const result = await editor.spawnProcess("git", [
    "show",
    "--stat",
    "--patch",
    hash,
  ]);

  if (result.exit_code !== 0) {
    return `Error fetching diff: ${result.stderr}`;
  }

  return result.stdout;
}

// =============================================================================
// Git Log View
// =============================================================================

function formatCommitRow(commit: GitCommit): string {
  // Build the line parts
  let line = "";

  // Add hash
  line += commit.shortHash + " ";

  // Add refs if present and enabled
  if (gitLogState.options.showRefs && commit.refs) {
    line += commit.refs + " ";
  }

  // Add subject
  line += commit.subject;

  return line + "\n";
}

function buildGitLogEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Magit-style header
  entries.push({
    text: "Commits:\n",
    properties: { type: "section-header" },
  });

  if (gitLogState.commits.length === 0) {
    entries.push({
      text: "  No commits found\n",
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
    text: `${gitLogState.commits.length} commits | ↑/↓/j/k: navigate | RET: show | y: yank hash | r: refresh | q: quit\n`,
    properties: { type: "footer" },
  });

  return entries;
}

function applyGitLogHighlighting(): void {
  if (gitLogState.bufferId === null) return;

  const bufferId = gitLogState.bufferId;

  // Clear existing overlays
  editor.removeOverlaysByPrefix(bufferId, "gitlog-");

  // Get buffer content to find positions for highlighting
  const bufferLength = editor.getBufferLength(bufferId);
  const content = editor.getBufferText(bufferId, 0, bufferLength);
  const lines = content.split("\n");

  // Get cursor line to highlight current row
  const cursorLine = editor.getCursorLine();
  const headerLines = 1; // Just "Commits:" header

  let byteOffset = 0;

  for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
    const line = lines[lineIdx];

    // Highlight section header
    if (line === "Commits:") {
      editor.addOverlay(
        bufferId,
        `gitlog-section-${lineIdx}`,
        byteOffset,
        byteOffset + line.length,
        colors.header[0],
        colors.header[1],
        colors.header[2],
        true // underline
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
    const isCurrentLine = lineIdx === cursorLine;

    // Highlight entire line if cursor is on it (using selected color with underline)
    if (isCurrentLine) {
      editor.addOverlay(
        bufferId,
        `gitlog-cursorline-${lineIdx}`,
        byteOffset,
        byteOffset + line.length,
        colors.selected[0],
        colors.selected[1],
        colors.selected[2],
        true, // underline to make it visible
        true  // bold
      );
    }

    // Find and highlight different parts of the line
    let pos = 0;

    // Highlight hash
    const hashStart = byteOffset + pos;
    const hashEnd = hashStart + commit.shortHash.length;
    editor.addOverlay(
      bufferId,
      `gitlog-hash-${lineIdx}`,
      hashStart,
      hashEnd,
      colors.hash[0],
      colors.hash[1],
      colors.hash[2],
      false
    );
    pos += commit.shortHash.length + 1;

    // Highlight refs (branches/tags)
    if (gitLogState.options.showRefs && commit.refs) {
      const refsStart = byteOffset + pos;
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
        `gitlog-refs-${lineIdx}`,
        refsStart,
        refsEnd,
        refColor[0],
        refColor[1],
        refColor[2],
        false
      );
    }

    byteOffset += line.length + 1;
  }
}

function updateGitLogView(): void {
  if (gitLogState.bufferId !== null) {
    const entries = buildGitLogEntries();
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
    text: `↑/↓/j/k: navigate | RET: open file at line | q: back to log\n`,
    properties: { type: "footer" },
  });

  return entries;
}

function applyCommitDetailHighlighting(): void {
  if (commitDetailState.bufferId === null) return;

  const bufferId = commitDetailState.bufferId;

  // Clear existing overlays
  editor.removeOverlaysByPrefix(bufferId, "gitdetail-");

  // Get buffer content
  const bufferLength = editor.getBufferLength(bufferId);
  const content = editor.getBufferText(bufferId, 0, bufferLength);
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
        `gitdetail-add-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.diffAdd[0],
        colors.diffAdd[1],
        colors.diffAdd[2],
        false
      );
    }
    // Highlight diff deletions (red)
    else if (line.startsWith("-") && !line.startsWith("---")) {
      editor.addOverlay(
        bufferId,
        `gitdetail-del-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.diffDel[0],
        colors.diffDel[1],
        colors.diffDel[2],
        false
      );
    }
    // Highlight hunk headers (blue)
    else if (line.startsWith("@@")) {
      editor.addOverlay(
        bufferId,
        `gitdetail-hunk-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.diffHunk[0],
        colors.diffHunk[1],
        colors.diffHunk[2],
        false
      );
    }
    // Highlight commit hash in "commit <hash>" line (git show format)
    else if (line.startsWith("commit ")) {
      const hashMatch = line.match(/^commit ([a-f0-9]+)/);
      if (hashMatch) {
        const hashStart = lineStart + 7; // "commit " is 7 chars
        editor.addOverlay(
          bufferId,
          `gitdetail-hash-${lineIdx}`,
          hashStart,
          hashStart + hashMatch[1].length,
          colors.hash[0],
          colors.hash[1],
          colors.hash[2],
          true // bold
        );
      }
    }
    // Highlight author line
    else if (line.startsWith("Author:")) {
      editor.addOverlay(
        bufferId,
        `gitdetail-author-${lineIdx}`,
        lineStart + 8, // "Author: " is 8 chars
        lineEnd,
        colors.author[0],
        colors.author[1],
        colors.author[2],
        false
      );
    }
    // Highlight date line
    else if (line.startsWith("Date:")) {
      editor.addOverlay(
        bufferId,
        `gitdetail-date-${lineIdx}`,
        lineStart + 6, // "Date: " is 6 chars (with trailing spaces it's 8)
        lineEnd,
        colors.date[0],
        colors.date[1],
        colors.date[2],
        false
      );
    }
    // Highlight diff file headers
    else if (line.startsWith("diff --git")) {
      editor.addOverlay(
        bufferId,
        `gitdetail-diffheader-${lineIdx}`,
        lineStart,
        lineEnd,
        colors.header[0],
        colors.header[1],
        colors.header[2],
        true // bold
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
    editor.setStatus("Git log already open");
    return;
  }

  editor.setStatus("Loading git log...");

  // Store the current split ID and buffer ID before opening git log
  gitLogState.splitId = editor.getActiveSplitId();
  gitLogState.sourceBufferId = editor.getActiveBufferId();

  // Fetch commits
  gitLogState.commits = await fetchGitLog();

  if (gitLogState.commits.length === 0) {
    editor.setStatus("No commits found or not a git repository");
    gitLogState.splitId = null;
    return;
  }

  // Build entries
  const entries = buildGitLogEntries();

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

    editor.setStatus(`Git log: ${gitLogState.commits.length} commits | ↑/↓: navigate | RET: show | q: quit`);
    editor.debug("Git log panel opened");
  } else {
    gitLogState.splitId = null;
    editor.setStatus("Failed to open git log panel");
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
  editor.setStatus("Git log closed");
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
    editor.setStatus(`Commit ${commitIndex + 1}/${gitLogState.commits.length}`);
  }
};

// Register cursor movement handler
editor.on("cursor_moved", "on_git_log_cursor_moved");

globalThis.git_log_refresh = async function(): Promise<void> {
  if (!gitLogState.isOpen) return;

  editor.setStatus("Refreshing git log...");
  gitLogState.commits = await fetchGitLog();
  updateGitLogView();
  editor.setStatus(`Git log refreshed: ${gitLogState.commits.length} commits`);
};

// Helper function to get commit at current cursor position
function getCommitAtCursor(): GitCommit | null {
  if (gitLogState.bufferId === null) return null;

  const cursorLine = editor.getCursorLine();
  const headerLines = 1;
  const commitIndex = cursorLine - headerLines;

  if (commitIndex >= 0 && commitIndex < gitLogState.commits.length) {
    return gitLogState.commits[commitIndex];
  }
  return null;
}

globalThis.git_log_show_commit = async function(): Promise<void> {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;
  if (gitLogState.splitId === null) return;

  const commit = getCommitAtCursor();
  if (!commit) {
    editor.setStatus("Move cursor to a commit line");
    return;
  }

  editor.setStatus(`Loading commit ${commit.shortHash}...`);

  // Fetch full commit info using git show (includes header and diff)
  const showOutput = await fetchCommitDiff(commit.hash);

  // Build entries using raw git show output
  const entries = buildCommitDetailEntries(commit, showOutput);

  // Create virtual buffer in the current split (replacing git log view)
  const bufferId = await editor.createVirtualBufferInExistingSplit({
    name: `*Commit: ${commit.shortHash}*`,
    mode: "git-commit-detail",
    read_only: true,
    entries: entries,
    split_id: gitLogState.splitId!,
    show_line_numbers: true, // Enable line numbers for diff navigation
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

    editor.setStatus(`Commit ${commit.shortHash} | ↑/↓: navigate | RET: open file | q: back`);
  } else {
    editor.setStatus("Failed to open commit details");
  }
};

globalThis.git_log_copy_hash = function(): void {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;

  const commit = getCommitAtCursor();
  if (!commit) {
    editor.setStatus("Move cursor to a commit line");
    return;
  }

  // Use spawn to copy to clipboard (works on most systems)
  // Try xclip first (Linux), then pbcopy (macOS), then xsel
  editor.spawnProcess("sh", ["-c", `echo -n "${commit.hash}" | xclip -selection clipboard 2>/dev/null || echo -n "${commit.hash}" | pbcopy 2>/dev/null || echo -n "${commit.hash}" | xsel --clipboard 2>/dev/null`])
    .then(() => {
      editor.setStatus(`Copied: ${commit.shortHash} (${commit.hash})`);
    })
    .catch(() => {
      // If all clipboard commands fail, just show the hash
      editor.setStatus(`Hash: ${commit.hash}`);
    });
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

  editor.setStatus(`Git log: ${gitLogState.commits.length} commits | ↑/↓: navigate | RET: show | q: quit`);
};

// Open file at the current diff line position
globalThis.git_commit_detail_open_file = function(): void {
  if (!commitDetailState.isOpen || commitDetailState.bufferId === null) {
    return;
  }

  // Get text properties at cursor position to find file/line info
  const props = editor.getTextPropertiesAtCursor(commitDetailState.bufferId);

  if (props.length > 0) {
    const file = props[0].file as string | undefined;
    const line = props[0].line as number | undefined;

    if (file) {
      // Construct full path relative to cwd
      const cwd = editor.getCwd();
      const fullPath = file.startsWith("/") ? file : `${cwd}/${file}`;

      // Open the file at the specified line
      const targetLine = line || 1;
      const success = editor.openFile(fullPath, targetLine, 1);

      if (success) {
        editor.setStatus(`Opened ${file}:${targetLine}`);
      } else {
        editor.setStatus(`Failed to open ${file}`);
      }
    } else {
      editor.setStatus("Move cursor to a diff line with file context");
    }
  } else {
    editor.setStatus("Move cursor to a diff line");
  }
};

// =============================================================================
// Command Registration
// =============================================================================

editor.registerCommand(
  "Git Log",
  "Show git log in magit-style interface",
  "show_git_log",
  "normal"
);

editor.registerCommand(
  "Git Log: Close",
  "Close the git log panel",
  "git_log_close",
  "normal"
);

editor.registerCommand(
  "Git Log: Refresh",
  "Refresh the git log",
  "git_log_refresh",
  "normal"
);

// =============================================================================
// Plugin Initialization
// =============================================================================

editor.setStatus("Git Log plugin loaded (magit-style)");
editor.debug("Git Log plugin initialized - Use 'Git Log' command to open");
