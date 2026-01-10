/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Git Blame Plugin - Magit-style Git Blame Interface
 *
 * Provides an interactive git blame view using Virtual Lines (Emacs-like model):
 * - Virtual buffer contains pure file content (for syntax highlighting)
 * - Virtual lines are added above each blame block using addVirtualLine API
 * - Headers have dark gray background and no line numbers
 * - Content lines preserve source line numbers and syntax highlighting
 *
 * This uses the persistent state model where:
 * - Plugin adds virtual lines when blame data loads (async)
 * - Render loop reads virtual lines synchronously from memory
 * - No view transform hooks needed - eliminates frame lag issues
 *
 * Features:
 * - 'b' to go back in history (show blame at parent commit)
 * - 'q' to close the blame view
 * - 'y' to yank (copy) the commit hash at cursor
 *
 * Inspired by magit's git-blame-additions feature.
 */

// =============================================================================
// Types and Interfaces
// =============================================================================

interface BlameLine {
  hash: string;
  shortHash: string;
  author: string;
  authorTime: string;      // Unix timestamp
  relativeDate: string;
  summary: string;
  lineNumber: number;      // Original line number
  finalLineNumber: number; // Final line number in the file
  content: string;
}

interface BlameBlock {
  hash: string;
  shortHash: string;
  author: string;
  relativeDate: string;
  summary: string;
  lines: BlameLine[];
  startLine: number;       // First line number in block (1-indexed)
  endLine: number;         // Last line number in block (1-indexed)
  startByte: number;       // Start byte offset in the buffer
  endByte: number;         // End byte offset in the buffer
}

interface BlameState {
  isOpen: boolean;
  bufferId: number | null;
  splitId: number | null;
  sourceBufferId: number | null;  // The buffer that was open before blame
  sourceFilePath: string | null;  // Path to the file being blamed
  currentCommit: string | null;   // Current commit being viewed (null = HEAD)
  commitStack: string[];          // Stack of commits for navigation
  blocks: BlameBlock[];           // Blame blocks with byte offsets
  fileContent: string;            // Pure file content (for virtual buffer)
  lineByteOffsets: number[];      // Byte offset of each line start
}

// =============================================================================
// State Management
// =============================================================================

const blameState: BlameState = {
  isOpen: false,
  bufferId: null,
  splitId: null,
  sourceBufferId: null,
  sourceFilePath: null,
  currentCommit: null,
  commitStack: [],
  blocks: [],
  fileContent: "",
  lineByteOffsets: [],
};

// =============================================================================
// Color Definitions for Header Styling
// =============================================================================

const colors = {
  headerFg: [0, 0, 0] as [number, number, number],           // Black text
  headerBg: [200, 200, 200] as [number, number, number],     // Light gray background
};

// =============================================================================
// Mode Definition
// =============================================================================

editor.defineMode(
  "git-blame",
  "normal", // inherit from normal mode for cursor movement
  [
    ["b", "git_blame_go_back"],
    ["q", "git_blame_close"],
    ["Escape", "git_blame_close"],
    ["y", "git_blame_copy_hash"],
  ],
  true // read-only
);

// =============================================================================
// Git Blame Parsing
// =============================================================================

/**
 * Parse git blame --porcelain output
 */
async function fetchGitBlame(filePath: string, commit: string | null): Promise<BlameLine[]> {
  const args = ["blame", "--porcelain"];

  if (commit) {
    args.push(commit);
  }

  args.push("--", filePath);

  const result = await editor.spawnProcess("git", args);

  if (result.exit_code !== 0) {
    editor.setStatus(editor.t("status.git_error", { error: result.stderr }));
    return [];
  }

  const lines: BlameLine[] = [];
  const output = result.stdout;
  const outputLines = output.split("\n");

  let currentHash = "";
  let currentAuthor = "";
  let currentAuthorTime = "";
  let currentSummary = "";
  let currentOrigLine = 0;
  let currentFinalLine = 0;

  // Cache for commit info to avoid redundant parsing
  const commitInfo: Map<string, { author: string; authorTime: string; summary: string }> = new Map();

  for (let i = 0; i < outputLines.length; i++) {
    const line = outputLines[i];

    // Check for commit line: <hash> <orig-line> <final-line> [num-lines]
    const commitMatch = line.match(/^([a-f0-9]{40}) (\d+) (\d+)/);
    if (commitMatch) {
      currentHash = commitMatch[1];
      currentOrigLine = parseInt(commitMatch[2], 10);
      currentFinalLine = parseInt(commitMatch[3], 10);

      // Check cache for this commit's info
      const cached = commitInfo.get(currentHash);
      if (cached) {
        currentAuthor = cached.author;
        currentAuthorTime = cached.authorTime;
        currentSummary = cached.summary;
      }
      continue;
    }

    // Parse header fields
    if (line.startsWith("author ")) {
      currentAuthor = line.slice(7);
      continue;
    }
    if (line.startsWith("author-time ")) {
      currentAuthorTime = line.slice(12);
      continue;
    }
    if (line.startsWith("summary ")) {
      currentSummary = line.slice(8);
      // Cache this commit's info
      commitInfo.set(currentHash, {
        author: currentAuthor,
        authorTime: currentAuthorTime,
        summary: currentSummary,
      });
      continue;
    }

    // Content line (starts with tab)
    if (line.startsWith("\t")) {
      const content = line.slice(1);

      // Calculate relative date from author-time
      const relativeDate = formatRelativeDate(parseInt(currentAuthorTime, 10));

      lines.push({
        hash: currentHash,
        shortHash: currentHash.slice(0, 7),
        author: currentAuthor,
        authorTime: currentAuthorTime,
        relativeDate: relativeDate,
        summary: currentSummary,
        lineNumber: currentOrigLine,
        finalLineNumber: currentFinalLine,
        content: content,
      });
    }
  }

  return lines;
}

/**
 * Format a unix timestamp as a relative date string
 */
function formatRelativeDate(timestamp: number): string {
  const now = Math.floor(Date.now() / 1000);
  const diff = now - timestamp;

  if (diff < 60) {
    return editor.t("time.just_now");
  } else if (diff < 3600) {
    const count = Math.floor(diff / 60);
    return editor.t(count > 1 ? "time.minutes_ago_plural" : "time.minutes_ago", { count: String(count) });
  } else if (diff < 86400) {
    const count = Math.floor(diff / 3600);
    return editor.t(count > 1 ? "time.hours_ago_plural" : "time.hours_ago", { count: String(count) });
  } else if (diff < 604800) {
    const count = Math.floor(diff / 86400);
    return editor.t(count > 1 ? "time.days_ago_plural" : "time.days_ago", { count: String(count) });
  } else if (diff < 2592000) {
    const count = Math.floor(diff / 604800);
    return editor.t(count > 1 ? "time.weeks_ago_plural" : "time.weeks_ago", { count: String(count) });
  } else if (diff < 31536000) {
    const count = Math.floor(diff / 2592000);
    return editor.t(count > 1 ? "time.months_ago_plural" : "time.months_ago", { count: String(count) });
  } else {
    const count = Math.floor(diff / 31536000);
    return editor.t(count > 1 ? "time.years_ago_plural" : "time.years_ago", { count: String(count) });
  }
}

/**
 * Fetch file content at a specific commit (or HEAD)
 */
async function fetchFileContent(filePath: string, commit: string | null): Promise<string> {
  if (commit) {
    // Get historical file content
    const result = await editor.spawnProcess("git", ["show", `${commit}:${filePath}`]);
    if (result.exit_code === 0) {
      return result.stdout;
    }
  }

  // Get current file content using editor API (cross-platform)
  try {
    return await editor.readFile(filePath);
  } catch {
    return "";
  }
}

/**
 * Build line byte offset lookup table
 */
function buildLineByteOffsets(content: string): number[] {
  const offsets: number[] = [0]; // Line 1 starts at byte 0
  let byteOffset = 0;

  for (const char of content) {
    byteOffset += char.length; // In JS strings, each char is at least 1
    if (char === '\n') {
      offsets.push(byteOffset);
    }
  }

  return offsets;
}

/**
 * Get byte offset for a given line number (1-indexed)
 */
function getLineByteOffset(lineNum: number): number {
  if (lineNum <= 0) return 0;
  const idx = lineNum - 1;
  if (idx < blameState.lineByteOffsets.length) {
    return blameState.lineByteOffsets[idx];
  }
  // Return end of file if line number is out of range
  return blameState.fileContent.length;
}

/**
 * Group blame lines into blocks by commit, with byte offset information
 */
function groupIntoBlocks(lines: BlameLine[]): BlameBlock[] {
  const blocks: BlameBlock[] = [];
  let currentBlock: BlameBlock | null = null;

  for (const line of lines) {
    // Check if we need to start a new block
    if (!currentBlock || currentBlock.hash !== line.hash) {
      // Save previous block
      if (currentBlock && currentBlock.lines.length > 0) {
        currentBlock.endByte = getLineByteOffset(currentBlock.endLine + 1);
        blocks.push(currentBlock);
      }

      // Start new block
      currentBlock = {
        hash: line.hash,
        shortHash: line.shortHash,
        author: line.author,
        relativeDate: line.relativeDate,
        summary: line.summary,
        lines: [],
        startLine: line.finalLineNumber,
        endLine: line.finalLineNumber,
        startByte: getLineByteOffset(line.finalLineNumber),
        endByte: 0, // Will be set when block is complete
      };
    }

    currentBlock.lines.push(line);
    currentBlock.endLine = line.finalLineNumber;
  }

  // Don't forget the last block
  if (currentBlock && currentBlock.lines.length > 0) {
    currentBlock.endByte = getLineByteOffset(currentBlock.endLine + 1);
    blocks.push(currentBlock);
  }

  return blocks;
}

// =============================================================================
// Virtual Lines (Emacs-like persistent state model)
// =============================================================================

const BLAME_NAMESPACE = "git-blame";

/**
 * Format a header line for a blame block
 */
function formatBlockHeader(block: BlameBlock): string {
  // Truncate summary if too long
  const maxSummaryLen = 50;
  const summary = block.summary.length > maxSummaryLen
    ? block.summary.slice(0, maxSummaryLen - 3) + "..."
    : block.summary;

  return `── ${block.shortHash} (${block.author}, ${block.relativeDate}) "${summary}" ──`;
}

/**
 * Find which block (if any) starts at or before the given byte offset
 */
function findBlockForByteOffset(byteOffset: number): BlameBlock | null {
  for (const block of blameState.blocks) {
    if (byteOffset >= block.startByte && byteOffset < block.endByte) {
      return block;
    }
  }
  return null;
}

/**
 * Add virtual lines for all blame block headers
 * Called when blame data is loaded or updated
 */
function addBlameHeaders(): void {
  if (blameState.bufferId === null) return;

  // Clear existing headers first
  editor.clearVirtualTextNamespace(blameState.bufferId, BLAME_NAMESPACE);

  // Add a virtual line above each block
  for (const block of blameState.blocks) {
    const headerText = formatBlockHeader(block);

    editor.addVirtualLine(
      blameState.bufferId,
      block.startByte,        // anchor position
      headerText,             // text content
      colors.headerFg[0],     // fg_r
      colors.headerFg[1],     // fg_g
      colors.headerFg[2],     // fg_b
      colors.headerBg[0],     // bg_r
      colors.headerBg[1],     // bg_g
      colors.headerBg[2],     // bg_b
      true,                   // above (LineAbove)
      BLAME_NAMESPACE,        // namespace for bulk removal
      0                       // priority
    );
  }

  editor.debug(`Added ${blameState.blocks.length} blame header virtual lines`);
}

// =============================================================================
// Public Commands
// =============================================================================

/**
 * Show git blame for the current file
 */
globalThis.show_git_blame = async function(): Promise<void> {
  if (blameState.isOpen) {
    editor.setStatus(editor.t("status.already_open"));
    return;
  }

  // Get current file path
  const activeBufferId = editor.getActiveBufferId();
  const filePath = editor.getBufferPath(activeBufferId);
  if (!filePath || filePath === "") {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }

  editor.setStatus(editor.t("status.loading"));

  // Store state before opening blame
  blameState.splitId = editor.getActiveSplitId();
  blameState.sourceBufferId = activeBufferId;
  blameState.sourceFilePath = filePath;
  blameState.currentCommit = null;
  blameState.commitStack = [];

  // Fetch file content and blame data in parallel
  const [fileContent, blameLines] = await Promise.all([
    fetchFileContent(filePath, null),
    fetchGitBlame(filePath, null),
  ]);

  if (blameLines.length === 0) {
    editor.setStatus(editor.t("status.no_blame_info"));
    resetState();
    return;
  }

  // Store file content and build line offset table
  blameState.fileContent = fileContent;
  blameState.lineByteOffsets = buildLineByteOffsets(fileContent);

  // Group into blocks with byte offsets
  blameState.blocks = groupIntoBlocks(blameLines);

  // Get file extension for language detection
  const ext = filePath.includes('.') ? filePath.split('.').pop() : '';
  const bufferName = `*blame:${editor.pathBasename(filePath)}*`;

  // Create virtual buffer with PURE file content (for syntax highlighting)
  // Virtual lines will be added after buffer creation
  const entries: TextPropertyEntry[] = [];

  // We need to track which line belongs to which block for text properties
  let lineNum = 1;
  const contentLines = fileContent.split('\n');
  let byteOffset = 0;

  for (const line of contentLines) {
    // Find the block for this line
    const block = findBlockForByteOffset(byteOffset);

    entries.push({
      text: line + (lineNum < contentLines.length || fileContent.endsWith('\n') ? '\n' : ''),
      properties: {
        type: "content",
        hash: block?.hash ?? null,
        shortHash: block?.shortHash ?? null,
        lineNumber: lineNum,
      },
    });

    byteOffset += line.length + 1; // +1 for newline
    lineNum++;
  }

  // Create virtual buffer with the file content
  const bufferId = await editor.createVirtualBufferInExistingSplit({
    name: bufferName,
    mode: "git-blame",
    read_only: true,
    entries: entries,
    split_id: blameState.splitId!,
    show_line_numbers: true,  // We DO want line numbers (headers won't have them due to source_offset: null)
    show_cursors: true,
    editing_disabled: true,
  });

  if (bufferId !== null) {
    blameState.isOpen = true;
    blameState.bufferId = bufferId;

    // Add virtual lines for blame headers (persistent state model)
    addBlameHeaders();

    editor.setStatus(editor.t("status.blame_ready", { count: String(blameState.blocks.length) }));
    editor.debug("Git blame panel opened with virtual lines architecture");
  } else {
    resetState();
    editor.setStatus(editor.t("status.failed_open"));
  }
};

/**
 * Reset blame state
 */
function resetState(): void {
  blameState.splitId = null;
  blameState.sourceBufferId = null;
  blameState.sourceFilePath = null;
  blameState.currentCommit = null;
  blameState.commitStack = [];
  blameState.blocks = [];
  blameState.fileContent = "";
  blameState.lineByteOffsets = [];
}

/**
 * Close the git blame view
 */
globalThis.git_blame_close = function(): void {
  if (!blameState.isOpen) {
    return;
  }

  // Restore the original buffer in the split
  if (blameState.splitId !== null && blameState.sourceBufferId !== null) {
    editor.setSplitBuffer(blameState.splitId, blameState.sourceBufferId);
  }

  // Close the blame buffer
  if (blameState.bufferId !== null) {
    editor.closeBuffer(blameState.bufferId);
  }

  blameState.isOpen = false;
  blameState.bufferId = null;
  resetState();

  editor.setStatus(editor.t("status.closed"));
};

/**
 * Get the commit hash at the current cursor position
 */
function getCommitAtCursor(): string | null {
  if (blameState.bufferId === null) return null;

  const props = editor.getTextPropertiesAtCursor(blameState.bufferId);

  if (props.length > 0) {
    const hash = props[0].hash as string | undefined;
    if (hash) {
      return hash;
    }
  }

  return null;
}

/**
 * Navigate to blame at the parent commit of the current line's commit
 */
globalThis.git_blame_go_back = async function(): Promise<void> {
  if (!blameState.isOpen || !blameState.sourceFilePath) {
    return;
  }

  const currentHash = getCommitAtCursor();
  if (!currentHash) {
    editor.setStatus(editor.t("status.move_to_line"));
    return;
  }

  // Skip if this is the "not committed yet" hash (all zeros)
  if (currentHash === "0000000000000000000000000000000000000000") {
    editor.setStatus(editor.t("status.not_committed"));
    return;
  }

  editor.setStatus(editor.t("status.loading_parent", { hash: currentHash.slice(0, 7) }));

  // Get the parent commit
  const parentCommit = `${currentHash}^`;

  // Push current state to stack for potential future navigation
  if (blameState.currentCommit) {
    blameState.commitStack.push(blameState.currentCommit);
  } else {
    blameState.commitStack.push("HEAD");
  }

  // Fetch file content and blame at parent commit
  const [fileContent, blameLines] = await Promise.all([
    fetchFileContent(blameState.sourceFilePath, parentCommit),
    fetchGitBlame(blameState.sourceFilePath, parentCommit),
  ]);

  if (blameLines.length === 0) {
    // Pop the stack since we couldn't navigate
    blameState.commitStack.pop();
    editor.setStatus(editor.t("status.cannot_go_back", { hash: currentHash.slice(0, 7) }));
    return;
  }

  // Update state
  blameState.currentCommit = parentCommit;
  blameState.fileContent = fileContent;
  blameState.lineByteOffsets = buildLineByteOffsets(fileContent);
  blameState.blocks = groupIntoBlocks(blameLines);

  // Update virtual buffer content
  if (blameState.bufferId !== null) {
    const entries: TextPropertyEntry[] = [];
    let lineNum = 1;
    const contentLines = fileContent.split('\n');
    let byteOffset = 0;

    for (const line of contentLines) {
      const block = findBlockForByteOffset(byteOffset);

      entries.push({
        text: line + (lineNum < contentLines.length || fileContent.endsWith('\n') ? '\n' : ''),
        properties: {
          type: "content",
          hash: block?.hash ?? null,
          shortHash: block?.shortHash ?? null,
          lineNumber: lineNum,
        },
      });

      byteOffset += line.length + 1;
      lineNum++;
    }

    editor.setVirtualBufferContent(blameState.bufferId, entries);

    // Re-add virtual lines for the new blame data
    addBlameHeaders();
  }

  const depth = blameState.commitStack.length;
  editor.setStatus(editor.t("status.blame_at_parent", { hash: currentHash.slice(0, 7), depth: String(depth) }));
};

/**
 * Copy the commit hash at cursor to clipboard
 */
globalThis.git_blame_copy_hash = function(): void {
  if (!blameState.isOpen) return;

  const hash = getCommitAtCursor();
  if (!hash) {
    editor.setStatus(editor.t("status.move_to_line"));
    return;
  }

  // Skip if this is the "not committed yet" hash
  if (hash === "0000000000000000000000000000000000000000") {
    editor.setStatus(editor.t("status.not_committed"));
    return;
  }

  // Copy hash to clipboard
  editor.copyToClipboard(hash);
  editor.setStatus(editor.t("status.hash_copied", { short: hash.slice(0, 7), full: hash }));
};

// =============================================================================
// Command Registration
// =============================================================================

editor.registerCommand(
  "%cmd.git_blame",
  "%cmd.git_blame_desc",
  "show_git_blame",
  "normal"
);

editor.registerCommand(
  "%cmd.git_blame_close",
  "%cmd.git_blame_close_desc",
  "git_blame_close",
  "normal"
);

editor.registerCommand(
  "%cmd.git_blame_go_back",
  "%cmd.git_blame_go_back_desc",
  "git_blame_go_back",
  "normal"
);

// =============================================================================
// Plugin Initialization
// =============================================================================

editor.setStatus(editor.t("status.ready"));
editor.debug("Git Blame plugin initialized - Use 'Git Blame' command to open");
