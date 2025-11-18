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
  sourceSplitId: number | null; // The split where source code is displayed
  sourceBufferId: number | null; // The buffer that was in the source split (to restore later)
  commits: GitCommit[];
  selectedIndex: number;
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
  sourceSplitId: null,
  sourceBufferId: null,
  commits: [],
  selectedIndex: 0,
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

// Define git-log mode with navigation keybindings
editor.defineMode(
  "git-log",
  null, // no parent mode
  [
    ["Return", "git_log_show_commit"],
    ["Tab", "git_log_show_commit"],
    ["j", "git_log_next"],
    ["k", "git_log_prev"],
    ["n", "git_log_next"],
    ["p", "git_log_prev"],
    ["Down", "git_log_next"],
    ["Up", "git_log_prev"],
    ["g", "git_log_first"],
    ["M-<", "git_log_first"],
    ["G", "git_log_last"],
    ["M->", "git_log_last"],
    ["q", "git_log_close"],
    ["Escape", "git_log_close"],
    ["r", "git_log_refresh"],
    ["y", "git_log_copy_hash"],
  ],
  true // read-only
);

// Define git-commit-detail mode for viewing commit details
editor.defineMode(
  "git-commit-detail",
  null,
  [
    ["q", "git_commit_detail_close"],
    ["Escape", "git_commit_detail_close"],
    ["j", "move_down"],
    ["k", "move_up"],
    ["Down", "move_down"],
    ["Up", "move_up"],
    ["C-d", "scroll_half_page_down"],
    ["C-u", "scroll_half_page_up"],
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

function formatCommitRow(commit: GitCommit, index: number): string {
  const marker = index === gitLogState.selectedIndex ? "* " : "  ";

  // Build the line parts
  let line = marker;

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
        text: formatCommitRow(commit, i),
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
    text: `${gitLogState.commits.length} commits | RET: show | j/k/n/p: nav | g/G: first/last | y: yank hash | r: refresh | q: quit\n`,
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

  let byteOffset = 0;
  const headerLines = 1; // Just "Commits:" header

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

    // Skip non-commit lines
    if (lineIdx < headerLines || (!line.startsWith("  ") && !line.startsWith("* "))) {
      byteOffset += line.length + 1;
      continue;
    }

    const commitIndex = lineIdx - headerLines;
    if (commitIndex < 0 || commitIndex >= gitLogState.commits.length) {
      byteOffset += line.length + 1;
      continue;
    }

    const commit = gitLogState.commits[commitIndex];
    const isSelected = commitIndex === gitLogState.selectedIndex;

    // Find and highlight different parts of the line
    let pos = 2; // Skip marker

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
      pos += commit.refs.length + 1;
    }

    // Highlight selection marker
    if (isSelected) {
      editor.addOverlay(
        bufferId,
        `gitlog-selected-${lineIdx}`,
        byteOffset,
        byteOffset + 1,
        colors.header[0],
        colors.header[1],
        colors.header[2],
        true // underline
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

function buildCommitDetailEntries(commit: GitCommit, diff: string): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Header
  entries.push({
    text: "══════════════════════════════════════════════════════════════════════════════\n",
    properties: { type: "separator" },
  });
  entries.push({
    text: `  Commit: ${commit.hash}\n`,
    properties: { type: "header", hash: commit.hash },
  });
  entries.push({
    text: "══════════════════════════════════════════════════════════════════════════════\n",
    properties: { type: "separator" },
  });

  // Commit metadata
  entries.push({
    text: `Author: ${commit.author} <${commit.authorEmail}>\n`,
    properties: { type: "meta", field: "author" },
  });
  entries.push({
    text: `Date:   ${commit.date} (${commit.relativeDate})\n`,
    properties: { type: "meta", field: "date" },
  });
  entries.push({
    text: "\n",
    properties: { type: "blank" },
  });

  // Subject and body
  entries.push({
    text: `    ${commit.subject}\n`,
    properties: { type: "subject" },
  });

  if (commit.body) {
    entries.push({
      text: "\n",
      properties: { type: "blank" },
    });
    for (const line of commit.body.split("\n")) {
      entries.push({
        text: `    ${line}\n`,
        properties: { type: "body" },
      });
    }
  }

  // Diff section
  entries.push({
    text: "\n",
    properties: { type: "blank" },
  });
  entries.push({
    text: "──────────────────────────────────────────────────────────────────────────────\n",
    properties: { type: "separator" },
  });

  // Parse and add diff lines
  for (const line of diff.split("\n")) {
    let lineType = "diff";
    if (line.startsWith("+") && !line.startsWith("+++")) {
      lineType = "diff-add";
    } else if (line.startsWith("-") && !line.startsWith("---")) {
      lineType = "diff-del";
    } else if (line.startsWith("@@")) {
      lineType = "diff-hunk";
    } else if (line.startsWith("diff --git") || line.startsWith("index ")) {
      lineType = "diff-header";
    }

    entries.push({
      text: `${line}\n`,
      properties: { type: lineType },
    });
  }

  // Footer
  entries.push({
    text: "──────────────────────────────────────────────────────────────────────────────\n",
    properties: { type: "separator" },
  });
  entries.push({
    text: "  j/k: scroll | q: close\n",
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

    // Highlight diff additions
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
    // Highlight diff deletions
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
    // Highlight hunk headers
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
    // Highlight commit hash in header
    else if (line.includes("Commit:")) {
      const hashMatch = line.match(/([a-f0-9]{40})/);
      if (hashMatch) {
        const hashPos = line.indexOf(hashMatch[1]);
        editor.addOverlay(
          bufferId,
          `gitdetail-hash-${lineIdx}`,
          lineStart + hashPos,
          lineStart + hashPos + hashMatch[1].length,
          colors.hash[0],
          colors.hash[1],
          colors.hash[2],
          false
        );
      }
    }
    // Highlight author
    else if (line.startsWith("Author:")) {
      editor.addOverlay(
        bufferId,
        `gitdetail-author-${lineIdx}`,
        lineStart + 8,
        lineEnd,
        colors.author[0],
        colors.author[1],
        colors.author[2],
        false
      );
    }
    // Highlight date
    else if (line.startsWith("Date:")) {
      editor.addOverlay(
        bufferId,
        `gitdetail-date-${lineIdx}`,
        lineStart + 8,
        lineEnd,
        colors.date[0],
        colors.date[1],
        colors.date[2],
        false
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

  // Store the current split ID and buffer ID before creating the panel
  gitLogState.sourceSplitId = editor.getActiveSplitId();
  gitLogState.sourceBufferId = editor.getActiveBufferId();

  // Fetch commits
  gitLogState.commits = await fetchGitLog();
  gitLogState.selectedIndex = 0;

  if (gitLogState.commits.length === 0) {
    editor.setStatus("No commits found or not a git repository");
    gitLogState.sourceSplitId = null;
    return;
  }

  // Build entries
  const entries = buildGitLogEntries();

  // Create virtual buffer in split
  const bufferId = await editor.createVirtualBufferInSplit({
    name: "*Git Log*",
    mode: "git-log",
    read_only: true,
    entries: entries,
    ratio: 0.6, // Original takes 60%, git log takes 40%
    panel_id: "git-log-panel",
    show_line_numbers: false,
    show_cursors: true,
  });

  if (bufferId !== null) {
    gitLogState.isOpen = true;
    gitLogState.bufferId = bufferId;
    gitLogState.splitId = editor.getActiveSplitId(); // Capture the git log's split ID

    // Apply syntax highlighting
    applyGitLogHighlighting();

    editor.setStatus(`Git log: ${gitLogState.commits.length} commits | Press ? for help`);
    editor.debug("Git log panel opened");
  } else {
    gitLogState.sourceSplitId = null;
    editor.setStatus("Failed to open git log panel");
  }
};

globalThis.git_log_close = function(): void {
  if (!gitLogState.isOpen) {
    return;
  }

  gitLogState.isOpen = false;
  gitLogState.bufferId = null;
  gitLogState.splitId = null;
  gitLogState.sourceSplitId = null;
  gitLogState.sourceBufferId = null;
  gitLogState.commits = [];
  gitLogState.selectedIndex = 0;
  editor.setStatus("Git log closed");
};

globalThis.git_log_next = function(): void {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;

  gitLogState.selectedIndex = Math.min(
    gitLogState.selectedIndex + 1,
    gitLogState.commits.length - 1
  );
  updateGitLogView();
  editor.setStatus(`Commit ${gitLogState.selectedIndex + 1}/${gitLogState.commits.length}`);
};

globalThis.git_log_prev = function(): void {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;

  gitLogState.selectedIndex = Math.max(gitLogState.selectedIndex - 1, 0);
  updateGitLogView();
  editor.setStatus(`Commit ${gitLogState.selectedIndex + 1}/${gitLogState.commits.length}`);
};

globalThis.git_log_first = function(): void {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;

  gitLogState.selectedIndex = 0;
  updateGitLogView();
  editor.setStatus(`Commit 1/${gitLogState.commits.length}`);
};

globalThis.git_log_last = function(): void {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;

  gitLogState.selectedIndex = gitLogState.commits.length - 1;
  updateGitLogView();
  editor.setStatus(`Commit ${gitLogState.commits.length}/${gitLogState.commits.length}`);
};

globalThis.git_log_refresh = async function(): Promise<void> {
  if (!gitLogState.isOpen) return;

  editor.setStatus("Refreshing git log...");
  gitLogState.commits = await fetchGitLog();
  gitLogState.selectedIndex = Math.min(gitLogState.selectedIndex, gitLogState.commits.length - 1);
  updateGitLogView();
  editor.setStatus(`Git log refreshed: ${gitLogState.commits.length} commits`);
};

globalThis.git_log_show_commit = async function(): Promise<void> {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;
  if (gitLogState.sourceSplitId === null) return;

  const commit = gitLogState.commits[gitLogState.selectedIndex];
  if (!commit) return;

  editor.setStatus(`Loading commit ${commit.shortHash}...`);

  // Fetch diff
  const diff = await fetchCommitDiff(commit.hash);

  // Build entries
  const entries = buildCommitDetailEntries(commit, diff);

  // Create virtual buffer in the source split (upper split)
  const bufferId = await editor.createVirtualBufferInExistingSplit({
    name: `*Commit: ${commit.shortHash}*`,
    mode: "git-commit-detail",
    read_only: true,
    entries: entries,
    split_id: gitLogState.sourceSplitId,
    show_line_numbers: false,
    show_cursors: true,
  });

  if (bufferId !== null) {
    commitDetailState.isOpen = true;
    commitDetailState.bufferId = bufferId;
    commitDetailState.splitId = gitLogState.sourceSplitId;
    commitDetailState.commit = commit;

    // Apply syntax highlighting
    applyCommitDetailHighlighting();

    editor.setStatus(`Commit ${commit.shortHash} by ${commit.author}`);
  } else {
    editor.setStatus("Failed to open commit details");
  }
};

globalThis.git_log_copy_hash = function(): void {
  if (!gitLogState.isOpen || gitLogState.commits.length === 0) return;

  const commit = gitLogState.commits[gitLogState.selectedIndex];
  if (!commit) return;

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

  // First, restore the original buffer to the source split before closing the commit detail buffer
  if (commitDetailState.splitId !== null && gitLogState.sourceBufferId !== null) {
    editor.setSplitBuffer(commitDetailState.splitId, gitLogState.sourceBufferId);
  }

  // Now close the commit detail buffer (it's no longer displayed anywhere)
  if (commitDetailState.bufferId !== null) {
    editor.closeBuffer(commitDetailState.bufferId);
  }

  commitDetailState.isOpen = false;
  commitDetailState.bufferId = null;
  commitDetailState.splitId = null;
  commitDetailState.commit = null;

  // Return focus to the git log split
  if (gitLogState.splitId !== null) {
    editor.focusSplit(gitLogState.splitId);
  }

  editor.setStatus("Commit details closed");
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
