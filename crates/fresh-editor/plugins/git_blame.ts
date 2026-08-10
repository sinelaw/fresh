/// <reference path="./lib/fresh.d.ts" />
import {
  type GitRepo,
  git,
  repoRelativePath,
  resolveGitRepoForPath,
} from "./lib/git_repo.ts";

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

/**
 * One open blame view. Several can be open at once (e.g. blame on two
 * different files side by side), so each is keyed by its own virtual
 * buffer id in `blameInstances` rather than living in a single global.
 */
interface BlameInstance {
  bufferId: number;                // Blame virtual buffer id (the map key)
  splitId: number | null;          // Split the blame buffer lives in
  sourceBufferId: number | null;   // The buffer that was open before blame
  sourceFilePath: string | null;   // Path to the file being blamed
  repo: GitRepo;                   // Repo the blamed file lives in (its sub-project in a monorepo)
  currentCommit: string | null;    // Current commit being viewed (null = HEAD)
  commitStack: string[];           // Stack of commits for `b`-navigation
  blocks: BlameBlock[];            // Blame blocks with byte offsets
  fileContent: string;             // Pure file content (for virtual buffer)
  lineByteOffsets: number[];       // Byte offset of each line start
}

// =============================================================================
// State Management
// =============================================================================

// Every open blame view, keyed by its virtual buffer id. An empty map means
// no blame is open. Keying by buffer id is what lets several blame buffers
// coexist; the mode handlers (`q`/`b`/`y`) act on whichever one is focused.
const blameInstances: Map<number, BlameInstance> = new Map();

/** The blame instance for the currently-focused buffer, or null. */
function activeBlame(): BlameInstance | null {
  return blameInstances.get(editor.getActiveBufferId()) ?? null;
}

// =============================================================================
// Color Definitions for Header Styling
// =============================================================================

// Blame headers are rendered via `addVirtualLine`, which accepts theme
// keys directly — so we don't expose colors as plugin settings. Themes
// drive the look; if a theme lacks specific blame keys, these fall
// through to the editor's status-bar palette which is what every theme
// defines.
const HEADER_FG_KEY = "ui.status_bar_fg";
const HEADER_BG_KEY = "ui.status_bar_bg";

// =============================================================================
// Mode Definition
// =============================================================================

editor.defineMode(
  "git-blame",
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
async function fetchGitBlame(repo: GitRepo, filePath: string, commit: string | null): Promise<BlameLine[]> {
  const args = ["blame", "--porcelain"];

  if (commit) {
    args.push(commit);
  }

  args.push("--", filePath);

  // Runs in the resolved repo root so git blame works in monorepo
  // sub-projects where the editor's cwd is not itself a git repo.
  const result = await git(editor, repo, args);

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
async function fetchFileContent(repo: GitRepo, filePath: string, commit: string | null): Promise<string> {
  if (commit) {
    // Historical file content via `git show <rev>:<repo-relative-path>`, run in
    // the repo root. The repo-relative path (not an absolute one — `git show
    // <rev>:<abs-path>` is a fatal error) is what makes this resolve; it also
    // covers monorepo sub-projects where the editor cwd isn't a repo. A
    // non-zero exit falls through to the current working-tree content below.
    const rel = repoRelativePath(repo, filePath);
    const result = await git(editor, repo, ["show", `${commit}:${rel}`]);
    if (result.exit_code === 0) {
      return result.stdout;
    }
  }

  // Get current file content using editor API (cross-platform)
  try {
    return await editor.readFile(editor.authorityPath(filePath)) ?? "";
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
function getLineByteOffset(
  lineByteOffsets: number[],
  fileContentLength: number,
  lineNum: number,
): number {
  if (lineNum <= 0) return 0;
  const idx = lineNum - 1;
  if (idx < lineByteOffsets.length) {
    return lineByteOffsets[idx];
  }
  // Return end of file if line number is out of range
  return fileContentLength;
}

/**
 * Group blame lines into blocks by commit, with byte offset information.
 * Byte offsets are derived from the caller's line table / content length so
 * this stays free of any single global blame state.
 */
function groupIntoBlocks(
  lines: BlameLine[],
  lineByteOffsets: number[],
  fileContentLength: number,
): BlameBlock[] {
  const blocks: BlameBlock[] = [];
  let currentBlock: BlameBlock | null = null;

  for (const line of lines) {
    // Check if we need to start a new block
    if (!currentBlock || currentBlock.hash !== line.hash) {
      // Save previous block
      if (currentBlock && currentBlock.lines.length > 0) {
        currentBlock.endByte = getLineByteOffset(
          lineByteOffsets,
          fileContentLength,
          currentBlock.endLine + 1,
        );
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
        startByte: getLineByteOffset(
          lineByteOffsets,
          fileContentLength,
          line.finalLineNumber,
        ),
        endByte: 0, // Will be set when block is complete
      };
    }

    currentBlock.lines.push(line);
    currentBlock.endLine = line.finalLineNumber;
  }

  // Don't forget the last block
  if (currentBlock && currentBlock.lines.length > 0) {
    currentBlock.endByte = getLineByteOffset(
      lineByteOffsets,
      fileContentLength,
      currentBlock.endLine + 1,
    );
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
 * Find which block (if any) contains the given byte offset
 */
function findBlockForByteOffset(blocks: BlameBlock[], byteOffset: number): BlameBlock | null {
  for (const block of blocks) {
    if (byteOffset >= block.startByte && byteOffset < block.endByte) {
      return block;
    }
  }
  return null;
}

/**
 * Build the virtual-buffer content entries (one per source line) for a blame
 * instance, tagging each with the commit hash of its block for cursor
 * lookups. Shared by the initial open and `b`-navigation.
 */
function buildContentEntries(fileContent: string, blocks: BlameBlock[]): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];
  let lineNum = 1;
  const contentLines = fileContent.split('\n');
  let byteOffset = 0;

  for (const line of contentLines) {
    const block = findBlockForByteOffset(blocks, byteOffset);

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

  return entries;
}

/**
 * Add virtual lines for all blame block headers of an instance.
 * Called when blame data is loaded or updated.
 */
function addBlameHeaders(inst: BlameInstance): void {
  // Clear existing headers first
  editor.clearVirtualTextNamespace(inst.bufferId, BLAME_NAMESPACE);

  // Add a virtual line above each block. Pass theme keys so the headers
  // restyle automatically when the user switches themes.
  for (const block of inst.blocks) {
    const headerText = formatBlockHeader(block);

    editor.addVirtualLine(
      inst.bufferId,
      block.startByte,        // anchor position
      headerText,             // text content
      { fg: HEADER_FG_KEY, bg: HEADER_BG_KEY },
      true,                   // above (LineAbove)
      BLAME_NAMESPACE,        // namespace for bulk removal
      0                       // priority
    );
  }

  editor.debug(`Added ${inst.blocks.length} blame header virtual lines`);
}

// =============================================================================
// Public Commands
// =============================================================================

/**
 * Show git blame for the current file
 */
async function show_git_blame() : Promise<void> {
  const activeBufferId = editor.getActiveBufferId();

  // Re-running blame while focused on a blame buffer would recurse on the
  // virtual buffer; treat it as a no-op (use `b` to walk history instead).
  if (blameInstances.has(activeBufferId)) {
    editor.setStatus(editor.t("status.already_open"));
    return;
  }

  // Get current file path
  const filePath = editor.getBufferPath(activeBufferId);
  if (!filePath || filePath === "") {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }

  editor.setStatus(editor.t("status.loading"));

  const splitId = editor.getActiveSplitId();

  // Capture the source cursor's line number (0-indexed) before opening
  // blame so we can land on the same line in the blame view. We use the
  // line number rather than the byte position so the jump is robust to
  // unsaved edits in the source buffer (its bytes can diverge from the
  // on-disk file the blame view shows, but the line index is still the
  // most semantically meaningful anchor).
  const sourceCursorLine = editor.getCursorLine();

  // Resolve the file's repo once (its own sub-project in a monorepo); every
  // git call for this blame view runs there.
  const repo = await resolveGitRepoForPath(editor, filePath);
  if (!repo) {
    editor.setStatus(editor.t("status.no_blame_info"));
    return;
  }

  // Fetch file content and blame data in parallel
  const [fileContent, blameLines] = await Promise.all([
    fetchFileContent(repo, filePath, null),
    fetchGitBlame(repo, filePath, null),
  ]);

  if (blameLines.length === 0) {
    editor.setStatus(editor.t("status.no_blame_info"));
    return;
  }

  // Build the line offset table and group into blocks with byte offsets.
  const lineByteOffsets = buildLineByteOffsets(fileContent);
  const blocks = groupIntoBlocks(blameLines, lineByteOffsets, fileContent.length);

  const bufferName = `*blame:${editor.pathBasename(filePath)}*`;

  // Create virtual buffer with PURE file content (for syntax highlighting);
  // virtual-line headers are added after buffer creation.
  // Pass `initialCursorLine` so the host lands the cursor on the same line
  // the user was on in the source buffer before the new buffer becomes
  // active — this is the only race-free way to set the cursor, and using a
  // line index (rather than a byte offset) keeps the UTF-8 byte math on the
  // host side, where the buffer content is already in UTF-8 bytes.
  const entries = buildContentEntries(fileContent, blocks);

  const result = await editor.createVirtualBufferInExistingSplit({
    name: bufferName,
    mode: "git-blame",
    readOnly: true,
    entries: entries,
    splitId,
    showLineNumbers: true,  // We DO want line numbers (headers won't have them due to source_offset: null)
    showCursors: true,
    editingDisabled: true,
    initialCursorLine: sourceCursorLine,
  });

  if (result === null) {
    editor.setStatus(editor.t("status.failed_open"));
    return;
  }

  // Register the instance keyed by its own buffer id so it coexists with
  // any other open blame buffers.
  const inst: BlameInstance = {
    bufferId: result.bufferId,
    splitId,
    sourceBufferId: activeBufferId,
    sourceFilePath: filePath,
    repo,
    currentCommit: null,
    commitStack: [],
    blocks,
    fileContent,
    lineByteOffsets,
  };
  blameInstances.set(inst.bufferId, inst);

  // Add virtual lines for blame headers (persistent state model)
  addBlameHeaders(inst);

  // Centre the cursor's line in the viewport. The cursor itself was placed
  // by the host via `initialCursorLine` (race-free, multi-byte-correct).
  if (splitId !== null) {
    editor.scrollToLineCenter(splitId, result.bufferId, sourceCursorLine);
  }

  editor.setStatus(editor.t("status.blame_ready", { count: String(blocks.length) }));
  editor.debug("Git blame panel opened with virtual lines architecture");
}
registerHandler("show_git_blame", show_git_blame);

/**
 * Close the focused git blame view (no-op if the focused buffer isn't one).
 */
function git_blame_close() : void {
  const inst = activeBlame();
  if (!inst) {
    return;
  }

  // Restore the original buffer in the split
  if (inst.splitId !== null && inst.sourceBufferId !== null) {
    editor.setSplitBuffer(inst.splitId, inst.sourceBufferId);
  }

  // Drop the instance before closing so the `buffer_closed` hook is a no-op.
  blameInstances.delete(inst.bufferId);
  editor.closeBuffer(inst.bufferId);

  editor.setStatus(editor.t("status.closed"));
}
registerHandler("git_blame_close", git_blame_close);

/**
 * Drop a blame instance when its buffer is closed by any path other than
 * `git_blame_close` — e.g. the user runs "Close Buffer"/"Close Tab" on the
 * blame tab, or the split it lives in is torn down. Without this, the dead
 * buffer id would linger in `blameInstances` and a fresh blame on the same
 * file could be mishandled (the reported "already open" bug).
 */
function on_git_blame_buffer_closed(data: { buffer_id: number }): void {
  blameInstances.delete(data.buffer_id);
}
registerHandler("on_git_blame_buffer_closed", on_git_blame_buffer_closed);
editor.on("buffer_closed", on_git_blame_buffer_closed);

/**
 * Get the commit hash at the cursor position in the given blame buffer
 */
function getCommitAtCursor(bufferId: number): string | null {
  const props = editor.getTextPropertiesAtCursor(bufferId);

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
async function git_blame_go_back() : Promise<void> {
  // Capture the focused instance up front so async work stays bound to it
  // even if focus moves to another blame buffer mid-await.
  const inst = activeBlame();
  if (!inst || !inst.sourceFilePath) {
    return;
  }

  const currentHash = getCommitAtCursor(inst.bufferId);
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
  if (inst.currentCommit) {
    inst.commitStack.push(inst.currentCommit);
  } else {
    inst.commitStack.push("HEAD");
  }

  // Fetch file content and blame at parent commit
  const [fileContent, blameLines] = await Promise.all([
    fetchFileContent(inst.repo, inst.sourceFilePath, parentCommit),
    fetchGitBlame(inst.repo, inst.sourceFilePath, parentCommit),
  ]);

  if (blameLines.length === 0) {
    // Pop the stack since we couldn't navigate
    inst.commitStack.pop();
    editor.setStatus(editor.t("status.cannot_go_back", { hash: currentHash.slice(0, 7) }));
    return;
  }

  // Update the instance's state
  inst.currentCommit = parentCommit;
  inst.fileContent = fileContent;
  inst.lineByteOffsets = buildLineByteOffsets(fileContent);
  inst.blocks = groupIntoBlocks(blameLines, inst.lineByteOffsets, fileContent.length);

  // Update virtual buffer content + re-add the headers for the new data.
  const entries = buildContentEntries(fileContent, inst.blocks);
  editor.setVirtualBufferContent(inst.bufferId, entries);
  addBlameHeaders(inst);

  const depth = inst.commitStack.length;
  editor.setStatus(editor.t("status.blame_at_parent", { hash: currentHash.slice(0, 7), depth: String(depth) }));
}
registerHandler("git_blame_go_back", git_blame_go_back);

/**
 * Copy the commit hash at cursor to clipboard
 */
function git_blame_copy_hash() : void {
  const inst = activeBlame();
  if (!inst) return;

  const hash = getCommitAtCursor(inst.bufferId);
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
}
registerHandler("git_blame_copy_hash", git_blame_copy_hash);

// =============================================================================
// Command Registration
// =============================================================================

editor.registerCommand(
  "%cmd.git_blame",
  "%cmd.git_blame_desc",
  "show_git_blame",
  null
);

editor.registerCommand(
  "%cmd.git_blame_close",
  "%cmd.git_blame_close_desc",
  "git_blame_close",
  null
);

editor.registerCommand(
  "%cmd.git_blame_go_back",
  "%cmd.git_blame_go_back_desc",
  "git_blame_go_back",
  null
);

// =============================================================================
// Plugin Initialization
// =============================================================================

editor.debug("Git Blame plugin initialized - Use 'Git Blame' command to open");
