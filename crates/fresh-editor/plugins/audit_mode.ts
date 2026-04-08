/// <reference path="./lib/fresh.d.ts" />
/// <reference path="./lib/types.ts" />
/// <reference path="./lib/virtual-buffer-factory.ts" />

// Review Diff Plugin
// Provides a unified workflow for reviewing code changes (diffs, conflicts, AI outputs).
//
// TODO: This plugin has incomplete/broken functionality:
// - Uses editor.prompt() which doesn't exist in the API (needs event-based prompt)
// - Uses VirtualBufferOptions.read_only (should be readOnly)
// - References stop_review_diff which is undefined
const editor = getEditor();

import { createVirtualBufferFactory } from "./lib/virtual-buffer-factory.ts";
const VirtualBufferFactory = createVirtualBufferFactory(editor);

/**
 * Hunk status for staging
 */
type HunkStatus = 'pending' | 'staged' | 'discarded';

/**
 * Review status for a hunk
 */
type ReviewStatus = 'pending' | 'approved' | 'needs_changes' | 'rejected' | 'question';

/**
 * A review comment attached to a specific line in a file
 * Uses file line numbers (not hunk-relative) so comments survive rebases
 */
interface ReviewComment {
  id: string;
  hunk_id: string;            // For grouping, but line numbers are primary
  file: string;               // File path
  text: string;
  timestamp: string;
  // Line positioning using actual file line numbers
  old_line?: number;          // Line number in old file version (for - lines)
  new_line?: number;          // Line number in new file version (for + lines)
  line_content?: string;      // The actual line content for context/matching
  line_type?: 'add' | 'remove' | 'context';  // Type of line
  // Selection range (for multi-line comments)
  selection?: {
    start_line: number;       // Start line in file
    end_line: number;         // End line in file
    version: 'old' | 'new';   // Which file version
  };
}

/**
 * A diff hunk (block of changes)
 */
interface Hunk {
  id: string;
  file: string;
  range: { start: number; end: number };  // new file line range
  oldRange: { start: number; end: number };  // old file line range
  type: 'add' | 'remove' | 'modify';
  lines: string[];
  status: HunkStatus;
  reviewStatus: ReviewStatus;
  contextHeader: string;
  byteOffset: number; // Position in the virtual buffer
  gitStatus?: 'staged' | 'unstaged' | 'untracked';
}

/**
 * A file entry from git status --porcelain
 */
interface FileEntry {
  path: string;
  status: string;           // 'M', 'A', 'D', 'R', 'C', '?'
  category: 'staged' | 'unstaged' | 'untracked';
  origPath?: string;         // for renames/copies
}

/**
 * Review Session State
 */
interface ReviewState {
  hunks: Hunk[];
  hunkStatus: Record<string, HunkStatus>;
  comments: ReviewComment[];
  originalRequest?: string;
  overallFeedback?: string;
  reviewBufferId: number | null;
  // New magit-style state
  files: FileEntry[];
  selectedIndex: number;
  fileScrollOffset: number;
  diffScrollOffset: number;
  diffSelectedLine: number;
  viewportWidth: number;
  viewportHeight: number;
  focusPanel: 'files' | 'diff';
}

const state: ReviewState = {
  hunks: [],
  hunkStatus: {},
  comments: [],
  reviewBufferId: null,
  files: [],
  selectedIndex: 0,
  fileScrollOffset: 0,
  diffScrollOffset: 0,
  diffSelectedLine: 0,
  viewportWidth: 80,
  viewportHeight: 24,
  focusPanel: 'files',
};

// --- Refresh State ---

// --- Colors & Styles ---
// Colors use theme keys where possible, falling back to direct values
const STYLE_BORDER: OverlayColorSpec = "ui.split_separator_fg";
const STYLE_HEADER: OverlayColorSpec = "syntax.keyword";
const STYLE_FILE_NAME: OverlayColorSpec = "syntax.string";
const STYLE_ADD_BG: OverlayColorSpec = "editor.diff_add_bg";
const STYLE_REMOVE_BG: OverlayColorSpec = "editor.diff_remove_bg";
const STYLE_ADD_TEXT: OverlayColorSpec = "diagnostic.info_fg";
const STYLE_REMOVE_TEXT: OverlayColorSpec = "diagnostic.error_fg";
const STYLE_STAGED: OverlayColorSpec = "editor.line_number_fg";
const STYLE_DISCARDED: OverlayColorSpec = "diagnostic.error_fg";
const STYLE_SECTION_HEADER: OverlayColorSpec = "syntax.type";
const STYLE_COMMENT: OverlayColorSpec = "diagnostic.warning_fg";
const STYLE_COMMENT_BORDER: OverlayColorSpec = "ui.split_separator_fg";
const STYLE_APPROVED: OverlayColorSpec = "diagnostic.info_fg";
const STYLE_REJECTED: OverlayColorSpec = "diagnostic.error_fg";
const STYLE_QUESTION: OverlayColorSpec = "diagnostic.warning_fg";

/**
 * Calculate UTF-8 byte length of a string manually since TextEncoder is not available
 */
function getByteLength(str: string): number {
    let s = 0;
    for (let i = 0; i < str.length; i++) {
        const code = str.charCodeAt(i);
        if (code <= 0x7f) s += 1;
        else if (code <= 0x7ff) s += 2;
        else if (code >= 0xd800 && code <= 0xdfff) {
            s += 4; i++;
        } else s += 3;
    }
    return s;
}

// --- Diff Logic ---

interface DiffPart {
    text: string;
    type: 'added' | 'removed' | 'unchanged';
}

function diffStrings(oldStr: string, newStr: string): DiffPart[] {
    const n = oldStr.length;
    const m = newStr.length;
    const dp: number[][] = Array.from({ length: n + 1 }, () => new Array(m + 1).fill(0));

    for (let i = 1; i <= n; i++) {
        for (let j = 1; j <= m; j++) {
            if (oldStr[i - 1] === newStr[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
            }
        }
    }

    const result: DiffPart[] = [];
    let i = n, j = m;
    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && oldStr[i - 1] === newStr[j - 1]) {
            result.unshift({ text: oldStr[i - 1], type: 'unchanged' });
            i--; j--;
        } else if (j > 0 && (i === 0 || dp[i][j - 1] >= dp[i - 1][j])) {
            result.unshift({ text: newStr[j - 1], type: 'added' });
            j--;
        } else {
            result.unshift({ text: oldStr[i - 1], type: 'removed' });
            i--;
        }
    }

    const coalesced: DiffPart[] = [];
    for (const part of result) {
        const last = coalesced[coalesced.length - 1];
        if (last && last.type === part.type) {
            last.text += part.text;
        } else {
            coalesced.push(part);
        }
    }
    return coalesced;
}

function parseDiffOutput(stdout: string, gitStatus: 'staged' | 'unstaged' | 'untracked'): Hunk[] {
    const lines = stdout.split('\n');
    const hunks: Hunk[] = [];
    let currentFile = "";
    let currentHunk: Hunk | null = null;

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        if (line.startsWith('diff --git')) {
            const match = line.match(/diff --git a\/(.+) b\/(.+)/);
            if (match) {
                currentFile = match[2];
                currentHunk = null;
            }
        } else if (line.startsWith('@@')) {
            const match = line.match(/@@ -(\d+),?\d* \+(\d+),?\d* @@(.*)/);
            if (match && currentFile) {
                const oldStart = parseInt(match[1]);
                const newStart = parseInt(match[2]);
                currentHunk = {
                    id: `${currentFile}:${newStart}:${gitStatus}`,
                    file: currentFile,
                    range: { start: newStart, end: newStart },
                    oldRange: { start: oldStart, end: oldStart },
                    type: 'modify',
                    lines: [],
                    status: 'pending',
                    reviewStatus: 'pending',
                    contextHeader: match[3]?.trim() || "",
                    byteOffset: 0,
                    gitStatus
                };
                hunks.push(currentHunk);
            }
        } else if (currentHunk && (line.startsWith('+') || line.startsWith('-') || line.startsWith(' '))) {
            if (!line.startsWith('---') && !line.startsWith('+++')) {
                 currentHunk.lines.push(line);
            }
        }
    }
    return hunks;
}

// --- Git status detection ---

/**
 * Parse `git status --porcelain -z` output into FileEntry[].
 *
 * Format: each entry is "XY path\0" where X = index status, Y = worktree status.
 * Renames/copies add "origPath\0" after the entry.
 * A file can appear in BOTH staged and unstaged if both X and Y are set.
 */
function parseGitStatusPorcelain(raw: string): FileEntry[] {
    const files: FileEntry[] = [];
    if (!raw) return files;

    // Split on null bytes
    const parts = raw.split('\0');
    let i = 0;
    while (i < parts.length) {
        const entry = parts[i];
        if (entry.length < 3) { i++; continue; }

        const x = entry[0]; // index (staged) status
        const y = entry[1]; // worktree (unstaged) status
        // entry[2] is a space
        const path = entry.slice(3);

        if (!path) { i++; continue; }

        // Check for rename/copy — next part is the original path
        let origPath: string | undefined;
        if (x === 'R' || x === 'C' || y === 'R' || y === 'C') {
            i++;
            origPath = parts[i];
        }

        // Untracked files: XY = '??'
        if (x === '?' && y === '?') {
            files.push({ path, status: '?', category: 'untracked' });
            i++;
            continue;
        }

        // Ignored files: XY = '!!' — skip
        if (x === '!' && y === '!') {
            i++;
            continue;
        }

        // Staged changes: X is not ' ' and not '?'
        if (x !== ' ' && x !== '?') {
            files.push({ path, status: x, category: 'staged', origPath });
        }

        // Unstaged changes: Y is not ' ' and not '?'
        if (y !== ' ' && y !== '?') {
            files.push({ path, status: y, category: 'unstaged', origPath });
        }

        i++;
    }

    // Sort: staged → unstaged → untracked, then by filename
    const categoryOrder: Record<string, number> = { staged: 0, unstaged: 1, untracked: 2 };
    files.sort((a, b) => {
        const orderA = categoryOrder[a.category] ?? 2;
        const orderB = categoryOrder[b.category] ?? 2;
        if (orderA !== orderB) return orderA - orderB;
        return a.path.localeCompare(b.path);
    });

    return files;
}

/**
 * Single source of truth for changed files using `git status --porcelain -z`.
 */
async function getGitStatus(): Promise<FileEntry[]> {
    const result = await editor.spawnProcess("git", ["status", "--porcelain", "-z"]);
    if (result.exit_code !== 0) return [];
    return parseGitStatusPorcelain(result.stdout);
}

/**
 * Fetch unified diffs for the given file entries.
 * Groups by category to minimize git invocations.
 */
async function fetchDiffsForFiles(files: FileEntry[]): Promise<Hunk[]> {
    const allHunks: Hunk[] = [];

    const hasStaged = files.some(f => f.category === 'staged');
    const hasUnstaged = files.some(f => f.category === 'unstaged');
    const untrackedFiles = files.filter(f => f.category === 'untracked');

    // Staged diffs
    if (hasStaged) {
        const result = await editor.spawnProcess("git", ["diff", "--cached", "--unified=3"]);
        if (result.exit_code === 0 && result.stdout.trim()) {
            allHunks.push(...parseDiffOutput(result.stdout, 'staged'));
        }
    }

    // Unstaged diffs
    if (hasUnstaged) {
        const result = await editor.spawnProcess("git", ["diff", "--unified=3"]);
        if (result.exit_code === 0 && result.stdout.trim()) {
            allHunks.push(...parseDiffOutput(result.stdout, 'unstaged'));
        }
    }

    // Untracked file diffs
    for (const f of untrackedFiles) {
        const result = await editor.spawnProcess("git", [
            "diff", "--no-index", "--unified=3", "/dev/null", f.path
        ]);
        if (result.stdout.trim()) {
            const hunks = parseDiffOutput(result.stdout, 'untracked');
            for (const h of hunks) {
                h.file = f.path;
                h.id = `${f.path}:${h.range.start}:untracked`;
                h.type = 'add';
            }
            allHunks.push(...hunks);
        }
    }

    // Sort: staged → unstaged → untracked, then by filename
    const statusOrder: Record<string, number> = { staged: 0, unstaged: 1, untracked: 2 };
    allHunks.sort((a, b) => {
        const orderA = statusOrder[a.gitStatus || 'unstaged'];
        const orderB = statusOrder[b.gitStatus || 'unstaged'];
        if (orderA !== orderB) return orderA - orderB;
        return a.file.localeCompare(b.file);
    });

    return allHunks;
}

// --- New magit-style rendering (Step 2 of rewrite) ---

const STYLE_SELECTED_BG: OverlayColorSpec = "editor.selection_bg";
const STYLE_DIVIDER: OverlayColorSpec = "ui.split_separator_fg";
const STYLE_FOOTER: OverlayColorSpec = "ui.status_bar_fg";
const STYLE_HUNK_HEADER: OverlayColorSpec = "syntax.keyword";

interface ListLine {
    text: string;
    type: 'section-header' | 'file';
    fileIndex?: number;     // index into state.files[]
    style?: Partial<OverlayOptions>;
    inlineOverlays?: InlineOverlay[];
}

interface DiffLine {
    text: string;
    type: 'hunk-header' | 'add' | 'remove' | 'context' | 'empty' | 'comment';
    style?: Partial<OverlayOptions>;
    inlineOverlays?: InlineOverlay[];
    // Line metadata for comment attachment
    hunkId?: string;
    file?: string;
    lineType?: 'add' | 'remove' | 'context';
    oldLine?: number;
    newLine?: number;
    lineContent?: string;
}

/**
 * Build the file list lines for the left panel.
 * Returns section headers (not selectable) and file entries.
 */
function buildFileListLines(): ListLine[] {
    const lines: ListLine[] = [];
    let lastCategory: string | undefined;

    for (let i = 0; i < state.files.length; i++) {
        const f = state.files[i];
        // Section headers
        if (f.category !== lastCategory) {
            lastCategory = f.category;
            let label = '';
            if (f.category === 'staged')    label = editor.t("section.staged") || "Staged";
            else if (f.category === 'unstaged') label = editor.t("section.unstaged") || "Changes";
            else if (f.category === 'untracked') label = editor.t("section.untracked") || "Untracked";
            lines.push({
                text: `▸ ${label}`,
                type: 'section-header',
                style: { fg: STYLE_SECTION_HEADER, bold: true },
            });
        }

        // Status icon
        const statusIcon = f.status === '?' ? 'A' : f.status;
        const prefix = i === state.selectedIndex ? '>' : ' ';
        const filename = f.origPath ? `${f.origPath} → ${f.path}` : f.path;
        lines.push({
            text: `${prefix}${statusIcon}  ${filename}`,
            type: 'file',
            fileIndex: i,
        });
    }

    return lines;
}

/**
 * Build the diff lines for the right panel based on currently selected file.
 */
function buildDiffLines(rightWidth: number): DiffLine[] {
    const lines: DiffLine[] = [];
    if (state.files.length === 0) return lines;

    const selectedFile = state.files[state.selectedIndex];
    if (!selectedFile) return lines;

    // Find hunks matching the selected file and category
    const fileHunks = state.hunks.filter(
        h => h.file === selectedFile.path && h.gitStatus === selectedFile.category
    );

    if (fileHunks.length === 0) {
        if (selectedFile.status === 'R' && selectedFile.origPath) {
            lines.push({ text: `Renamed from ${selectedFile.origPath}`, type: 'empty', style: { fg: STYLE_SECTION_HEADER } });
        } else if (selectedFile.status === 'D') {
            lines.push({ text: "(file deleted)", type: 'empty' });
        } else if (selectedFile.status === 'T') {
            lines.push({ text: "(type change: file ↔ symlink)", type: 'empty', style: { fg: STYLE_SECTION_HEADER } });
        } else if (selectedFile.status === '?' && selectedFile.path.endsWith('/')) {
            lines.push({ text: "(untracked directory)", type: 'empty' });
        } else {
            lines.push({ text: "(no diff available)", type: 'empty' });
        }
        return lines;
    }

    for (const hunk of fileHunks) {
        // Hunk header
        const header = hunk.contextHeader
            ? `@@ ${hunk.contextHeader} @@`
            : `@@ -${hunk.oldRange.start} +${hunk.range.start} @@`;
        lines.push({
            text: header,
            type: 'hunk-header',
            hunkId: hunk.id,
            file: hunk.file,
            style: { fg: STYLE_HUNK_HEADER, bold: true },
        });

        // Track actual file line numbers as we iterate
        let oldLineNum = hunk.oldRange.start;
        let newLineNum = hunk.range.start;

        // Diff content lines — only set background color so the normal editor
        // foreground stays readable across all themes. The bg uses theme-aware
        // diff colors that each theme can customize.
        for (const line of hunk.lines) {
            const prefix = line[0];
            const lineType: 'add' | 'remove' | 'context' =
                prefix === '+' ? 'add' : prefix === '-' ? 'remove' : 'context';
            const curOldLine = lineType !== 'add' ? oldLineNum : undefined;
            const curNewLine = lineType !== 'remove' ? newLineNum : undefined;

            if (prefix === '+') {
                lines.push({
                    text: line,
                    type: 'add',
                    style: { bg: STYLE_ADD_BG, extendToLineEnd: true },
                    hunkId: hunk.id, file: hunk.file,
                    lineType, oldLine: curOldLine, newLine: curNewLine, lineContent: line,
                });
                newLineNum++;
            } else if (prefix === '-') {
                lines.push({
                    text: line,
                    type: 'remove',
                    style: { bg: STYLE_REMOVE_BG, extendToLineEnd: true },
                    hunkId: hunk.id, file: hunk.file,
                    lineType, oldLine: curOldLine, newLine: curNewLine, lineContent: line,
                });
                oldLineNum++;
            } else {
                lines.push({
                    text: line,
                    type: 'context',
                    hunkId: hunk.id, file: hunk.file,
                    lineType, oldLine: curOldLine, newLine: curNewLine, lineContent: line,
                });
                oldLineNum++;
                newLineNum++;
            }

            // Render inline comments attached to this line
            const lineComments = state.comments.filter(c =>
                c.hunk_id === hunk.id && (
                    (c.line_type === 'add' && c.new_line === curNewLine) ||
                    (c.line_type === 'remove' && c.old_line === curOldLine) ||
                    (c.line_type === 'context' && c.new_line === curNewLine)
                )
            );
            for (const comment of lineComments) {
                const lineRef = comment.line_type === 'add'
                    ? `+${comment.new_line}`
                    : comment.line_type === 'remove'
                    ? `-${comment.old_line}`
                    : `${comment.new_line}`;
                lines.push({
                    text: `  \u00bb [${lineRef}] ${comment.text}`,
                    type: 'comment',
                    style: { fg: STYLE_COMMENT, italic: true },
                });
            }
        }
    }

    return lines;
}

/**
 * Build the full display as exactly viewportHeight lines.
 * Layout:
 *   Row 0:        Toolbar (shortcuts)
 *   Row 1:        Header (left: GIT STATUS, right: DIFF FOR <file>)
 *   Rows 2..H-1:  Main content (left file list, │ divider, right diff)
 */
function buildMagitDisplayEntries(): TextPropertyEntry[] {
    const entries: TextPropertyEntry[] = [];
    const H = state.viewportHeight;
    const W = state.viewportWidth;
    const leftWidth = Math.max(28, Math.floor(W * 0.3));
    const rightWidth = W - leftWidth - 1; // 1 for divider

    const allFileLines = buildFileListLines();
    const diffLines = buildDiffLines(rightWidth);

    const mainRows = H - 2; // rows 2..H-1

    // --- File list scrolling ---
    let selectedLineIdx = -1;
    for (let i = 0; i < allFileLines.length; i++) {
        if (allFileLines[i].type === 'file' && allFileLines[i].fileIndex === state.selectedIndex) {
            selectedLineIdx = i;
            break;
        }
    }
    if (selectedLineIdx >= 0) {
        if (selectedLineIdx < state.fileScrollOffset) {
            state.fileScrollOffset = selectedLineIdx;
        }
        if (selectedLineIdx >= state.fileScrollOffset + mainRows) {
            state.fileScrollOffset = selectedLineIdx - mainRows + 1;
        }
    }
    const maxFileOffset = Math.max(0, allFileLines.length - mainRows);
    if (state.fileScrollOffset > maxFileOffset) state.fileScrollOffset = maxFileOffset;
    if (state.fileScrollOffset < 0) state.fileScrollOffset = 0;

    const visibleFileLines = allFileLines.slice(state.fileScrollOffset, state.fileScrollOffset + mainRows);

    // --- Diff scrolling & selected line clamping ---
    if (diffLines.length > 0) {
        if (state.diffSelectedLine >= diffLines.length) state.diffSelectedLine = diffLines.length - 1;
        if (state.diffSelectedLine < 0) state.diffSelectedLine = 0;
    } else {
        state.diffSelectedLine = 0;
    }
    const maxDiffOffset = Math.max(0, diffLines.length - mainRows);
    if (state.diffScrollOffset > maxDiffOffset) state.diffScrollOffset = maxDiffOffset;
    if (state.diffScrollOffset < 0) state.diffScrollOffset = 0;

    const visibleDiffLines = diffLines.slice(state.diffScrollOffset, state.diffScrollOffset + mainRows);

    // --- Row 0: Toolbar ---
    const toolbar = " [Tab] Switch Panel  [s] Stage  [u] Unstage  [d] Discard  [c] Comment  [Enter] Drill-Down  [r] Refresh";
    entries.push({
        text: toolbar.substring(0, W).padEnd(W) + "\n",
        style: { fg: STYLE_FOOTER, bg: "ui.status_bar_bg" as OverlayColorSpec, extendToLineEnd: true },
        properties: { type: "toolbar" },
    });

    // --- Row 1: Header ---
    const selectedFile = state.files[state.selectedIndex];
    const focusLeft = state.focusPanel === 'files';
    const leftHeader = " GIT STATUS";
    const rightHeader = selectedFile
        ? ` DIFF FOR ${selectedFile.path}`
        : " DIFF";
    const leftHeaderPadded = leftHeader.padEnd(leftWidth).substring(0, leftWidth);
    const rightHeaderPadded = rightHeader.substring(0, rightWidth);

    const leftHeaderStyle: Partial<OverlayOptions> = focusLeft
        ? { fg: STYLE_HEADER, bold: true, underline: true }
        : { fg: STYLE_DIVIDER };
    const rightHeaderStyle: Partial<OverlayOptions> = focusLeft
        ? { fg: STYLE_DIVIDER }
        : { fg: STYLE_HEADER, bold: true, underline: true };

    entries.push({ text: leftHeaderPadded, style: leftHeaderStyle, properties: { type: "header" } });
    entries.push({ text: "│", style: { fg: STYLE_DIVIDER }, properties: { type: "divider" } });
    entries.push({ text: rightHeaderPadded, style: rightHeaderStyle, properties: { type: "header" } });
    entries.push({ text: "\n", properties: { type: "newline" } });

    // --- Rows 2..H-1: Main content ---
    for (let i = 0; i < mainRows; i++) {
        const fileItem = visibleFileLines[i];
        const diffItem = visibleDiffLines[i];

        // Left panel
        const leftText = fileItem ? (" " + fileItem.text) : "";
        const leftPadded = leftText.padEnd(leftWidth).substring(0, leftWidth);
        const isSelected = fileItem?.type === 'file' && fileItem.fileIndex === state.selectedIndex;

        const leftEntry: TextPropertyEntry = {
            text: leftPadded,
            properties: {
                type: fileItem?.type || "blank",
                fileIndex: fileItem?.fileIndex,
            },
            style: fileItem?.style,
            inlineOverlays: fileItem?.inlineOverlays,
        };
        if (isSelected) {
            leftEntry.style = { ...(leftEntry.style || {}), bg: STYLE_SELECTED_BG, bold: true };
        }
        entries.push(leftEntry);

        // Divider
        entries.push({ text: "│", style: { fg: STYLE_DIVIDER }, properties: { type: "divider" } });

        // Right panel — when diff panel is focused, highlight the selected line
        const rightText = diffItem ? (" " + diffItem.text) : "";
        const rightTruncated = rightText.substring(0, rightWidth);
        const diffLineIndex = state.diffScrollOffset + i;
        const isDiffCursorLine = !focusLeft && diffLineIndex === state.diffSelectedLine && diffItem != null;
        const rightStyle = isDiffCursorLine
            ? { ...(diffItem?.style || {}), bg: STYLE_SELECTED_BG, extendToLineEnd: true }
            : diffItem?.style;
        entries.push({
            text: rightTruncated,
            properties: { type: diffItem?.type || "blank" },
            style: rightStyle,
            inlineOverlays: diffItem?.inlineOverlays,
        });

        // Newline
        entries.push({ text: "\n", properties: { type: "newline" } });
    }

    return entries;
}

/**
 * Ensure the diff scroll offset keeps the selected line visible.
 */
function scrollDiffToSelected(): void {
    const mainRows = state.viewportHeight - 2;
    if (state.diffSelectedLine < state.diffScrollOffset) {
        state.diffScrollOffset = state.diffSelectedLine;
    }
    if (state.diffSelectedLine >= state.diffScrollOffset + mainRows) {
        state.diffScrollOffset = state.diffSelectedLine - mainRows + 1;
    }
}

/**
 * Refresh the display — rebuild entries and set buffer content.
 * Always re-queries viewport dimensions to handle sidebar toggles and splits.
 */
function updateMagitDisplay(): void {
    if (state.reviewBufferId === null) return;
    refreshViewportDimensions();
    const entries = buildMagitDisplayEntries();
    editor.clearNamespace(state.reviewBufferId, "review-diff");
    editor.setVirtualBufferContent(state.reviewBufferId, entries);
}

function review_refresh() { refreshMagitData(); }
registerHandler("review_refresh", review_refresh);

// --- New magit navigation handlers (Step 3) ---

function review_nav_up() {
    if (state.focusPanel === 'files') {
        if (state.files.length === 0) return;
        if (state.selectedIndex > 0) {
            state.selectedIndex--;
            state.diffScrollOffset = 0;
            state.diffSelectedLine = 0;
            updateMagitDisplay();
        }
    } else {
        if (state.diffSelectedLine > 0) {
            state.diffSelectedLine--;
            scrollDiffToSelected();
        }
        updateMagitDisplay();
    }
}
registerHandler("review_nav_up", review_nav_up);

function review_nav_down() {
    if (state.focusPanel === 'files') {
        if (state.files.length === 0) return;
        if (state.selectedIndex < state.files.length - 1) {
            state.selectedIndex++;
            state.diffScrollOffset = 0;
            state.diffSelectedLine = 0;
            updateMagitDisplay();
        }
    } else {
        state.diffSelectedLine++;
        scrollDiffToSelected();
        updateMagitDisplay();
    }
}
registerHandler("review_nav_down", review_nav_down);

function review_page_up() {
    const mainRows = state.viewportHeight - 2;
    if (state.focusPanel === 'files') {
        if (state.selectedIndex > 0) {
            state.selectedIndex = Math.max(0, state.selectedIndex - mainRows);
            state.diffScrollOffset = 0;
            state.diffSelectedLine = 0;
            updateMagitDisplay();
        }
    } else {
        state.diffSelectedLine = Math.max(0, state.diffSelectedLine - mainRows);
        state.diffScrollOffset = Math.max(0, state.diffScrollOffset - mainRows);
        updateMagitDisplay();
    }
}
registerHandler("review_page_up", review_page_up);

function review_page_down() {
    const mainRows = state.viewportHeight - 2;
    if (state.focusPanel === 'files') {
        if (state.selectedIndex < state.files.length - 1) {
            state.selectedIndex = Math.min(state.files.length - 1, state.selectedIndex + mainRows);
            state.diffScrollOffset = 0;
            state.diffSelectedLine = 0;
            updateMagitDisplay();
        }
    } else {
        state.diffSelectedLine += mainRows;
        state.diffScrollOffset += mainRows;
        updateMagitDisplay();
    }
}
registerHandler("review_page_down", review_page_down);

function review_toggle_focus() {
    state.focusPanel = state.focusPanel === 'files' ? 'diff' : 'files';
    updateMagitDisplay();
}
registerHandler("review_toggle_focus", review_toggle_focus);

function review_focus_files() {
    if (state.focusPanel !== 'files') {
        state.focusPanel = 'files';
        updateMagitDisplay();
    }
}
registerHandler("review_focus_files", review_focus_files);

function review_focus_diff() {
    if (state.focusPanel !== 'diff') {
        state.focusPanel = 'diff';
        updateMagitDisplay();
    }
}
registerHandler("review_focus_diff", review_focus_diff);

function review_nav_home() {
    if (state.focusPanel === 'files') {
        if (state.files.length === 0) return;
        state.selectedIndex = 0;
        state.diffScrollOffset = 0;
        state.diffSelectedLine = 0;
        updateMagitDisplay();
    } else {
        state.diffScrollOffset = 0;
        state.diffSelectedLine = 0;
        updateMagitDisplay();
    }
}
registerHandler("review_nav_home", review_nav_home);

function review_nav_end() {
    if (state.focusPanel === 'files') {
        if (state.files.length === 0) return;
        state.selectedIndex = state.files.length - 1;
        state.diffScrollOffset = 0;
        state.diffSelectedLine = 0;
        updateMagitDisplay();
    } else {
        // Scroll diff to bottom
        const mainRows = state.viewportHeight - 2;
        const selectedFile = state.files[state.selectedIndex];
        if (selectedFile) {
            const diffLines = buildDiffLines(state.viewportWidth - Math.max(28, Math.floor(state.viewportWidth * 0.3)) - 1);
            state.diffSelectedLine = Math.max(0, diffLines.length - 1);
            state.diffScrollOffset = Math.max(0, diffLines.length - mainRows);
        }
        updateMagitDisplay();
    }
}
registerHandler("review_nav_end", review_nav_end);

// --- Real git stage/unstage/discard actions (Step 4) ---

async function review_stage_file() {
    if (state.files.length === 0) return;
    const f = state.files[state.selectedIndex];
    if (!f) return;
    await editor.spawnProcess("git", ["add", "--", f.path]);
    await refreshMagitData();
}
registerHandler("review_stage_file", review_stage_file);

async function review_unstage_file() {
    if (state.files.length === 0) return;
    const f = state.files[state.selectedIndex];
    if (!f) return;
    await editor.spawnProcess("git", ["reset", "HEAD", "--", f.path]);
    await refreshMagitData();
}
registerHandler("review_unstage_file", review_unstage_file);

function review_discard_file() {
    if (state.files.length === 0) return;
    const f = state.files[state.selectedIndex];
    if (!f) return;

    // Show confirmation prompt — discard is destructive and irreversible
    const action = f.category === 'untracked' ? "Delete" : "Discard changes in";
    editor.startPrompt(`${action} "${f.path}"? This cannot be undone.`, "review-discard-confirm");
    const suggestions: PromptSuggestion[] = [
        { text: `${action} file`, description: "Permanently lose changes", value: "discard" },
        { text: "Cancel", description: "Keep the file as-is", value: "cancel" },
    ];
    editor.setPromptSuggestions(suggestions);
}
registerHandler("review_discard_file", review_discard_file);

async function on_review_discard_confirm(args: { prompt_type: string; input: string; selected_index: number | null }): Promise<boolean> {
    if (args.prompt_type !== "review-discard-confirm") return true;

    const response = args.input.trim().toLowerCase();
    if (response === "discard" || args.selected_index === 0) {
        const f = state.files[state.selectedIndex];
        if (f) {
            if (f.category === 'untracked') {
                await editor.spawnProcess("rm", ["--", f.path]);
            } else {
                await editor.spawnProcess("git", ["checkout", "--", f.path]);
            }
            await refreshMagitData();
            editor.setStatus(`Discarded: ${f.path}`);
        }
    } else {
        editor.setStatus("Discard cancelled");
    }
    return false;
}
registerHandler("on_review_discard_confirm", on_review_discard_confirm);

/**
 * Refresh file list and diffs using the new git status approach, then re-render.
 */
async function refreshMagitData() {
    const files = await getGitStatus();
    state.files = files;
    state.hunks = await fetchDiffsForFiles(files);
    // Clamp selectedIndex
    if (state.selectedIndex >= state.files.length) {
        state.selectedIndex = Math.max(0, state.files.length - 1);
    }
    state.diffScrollOffset = 0;
    state.diffSelectedLine = 0;
    updateMagitDisplay();
}

// --- Resize handler ---

/**
 * Refresh viewport dimensions from the actual split viewport.
 * This accounts for sidebars (file explorer) that reduce available width,
 * unlike the terminal-level resize event which reports full terminal size.
 */
function refreshViewportDimensions(): boolean {
    const viewport = editor.getViewport();
    if (viewport) {
        const changed = viewport.width !== state.viewportWidth || viewport.height !== state.viewportHeight;
        state.viewportWidth = viewport.width;
        state.viewportHeight = viewport.height;
        return changed;
    }
    return false;
}

function onReviewDiffResize(_data: { width: number; height: number }): void {
    if (state.reviewBufferId === null) return;
    refreshViewportDimensions();
    updateMagitDisplay();
}
registerHandler("onReviewDiffResize", onReviewDiffResize);

let activeDiffViewState: { lSplit: number, rSplit: number } | null = null;

/**
 * Find line number for a given byte offset using binary search
 */
function findLineForByte(lineByteOffsets: number[], topByte: number): number {
    let low = 0;
    let high = lineByteOffsets.length - 1;
    while (low < high) {
        const mid = Math.floor((low + high + 1) / 2);
        if (lineByteOffsets[mid] <= topByte) {
            low = mid;
        } else {
            high = mid - 1;
        }
    }
    return low;
}

function on_viewport_changed(data: any) {
    // This handler is now a no-op - scroll sync is handled by the core
    // using the anchor-based ScrollSyncGroup system.
    // Keeping the handler for backward compatibility if core sync fails.
    if (!activeDiffViewState || !activeSideBySideState) return;

    // Skip if core scroll sync is active (we have a scrollSyncGroupId)
    if (activeSideBySideState.scrollSyncGroupId !== null) return;

    const { oldSplitId, newSplitId, oldLineByteOffsets, newLineByteOffsets } = activeSideBySideState;

    if (data.splitId === oldSplitId && newLineByteOffsets.length > 0) {
        // OLD pane scrolled - find which line it's on and sync NEW pane to same line
        const lineNum = findLineForByte(oldLineByteOffsets, data.top_byte);
        const targetByte = newLineByteOffsets[Math.min(lineNum, newLineByteOffsets.length - 1)];
        (editor as any).setSplitScroll(newSplitId, targetByte);
    } else if (data.splitId === newSplitId && oldLineByteOffsets.length > 0) {
        // NEW pane scrolled - find which line it's on and sync OLD pane to same line
        const lineNum = findLineForByte(newLineByteOffsets, data.top_byte);
        const targetByte = oldLineByteOffsets[Math.min(lineNum, oldLineByteOffsets.length - 1)];
        (editor as any).setSplitScroll(oldSplitId, targetByte);
    }
}
registerHandler("on_viewport_changed", on_viewport_changed);

/**
 * Represents an aligned line pair for side-by-side diff display
 */
interface AlignedLine {
    oldLine: string | null;  // null means filler line
    newLine: string | null;  // null means filler line
    oldLineNum: number | null;
    newLineNum: number | null;
    changeType: 'unchanged' | 'added' | 'removed' | 'modified';
}

/**
 * Parse git diff and compute fully aligned line pairs for side-by-side display.
 * Shows the complete files with proper alignment through all hunks.
 */
function computeFullFileAlignedDiff(oldContent: string, newContent: string, hunks: Hunk[]): AlignedLine[] {
    const oldLines = oldContent.split('\n');
    const newLines = newContent.split('\n');
    const aligned: AlignedLine[] = [];

    // Build a map of changes from all hunks for this file
    // Key: old line number (1-based), Value: { type, newLineNum, content }
    interface ChangeInfo {
        type: 'removed' | 'added' | 'modified' | 'context';
        oldContent?: string;
        newContent?: string;
        newLineNum?: number;
    }

    // Parse all hunks for this file
    const allHunkChanges: { oldStart: number, newStart: number, changes: { type: 'add' | 'remove' | 'context', content: string }[] }[] = [];
    for (const hunk of hunks) {
        const changes: { type: 'add' | 'remove' | 'context', content: string }[] = [];
        for (const line of hunk.lines) {
            if (line.startsWith('+')) {
                changes.push({ type: 'add', content: line.substring(1) });
            } else if (line.startsWith('-')) {
                changes.push({ type: 'remove', content: line.substring(1) });
            } else if (line.startsWith(' ')) {
                changes.push({ type: 'context', content: line.substring(1) });
            }
        }
        allHunkChanges.push({
            oldStart: hunk.oldRange.start,
            newStart: hunk.range.start,
            changes
        });
    }

    // Sort hunks by old line start
    allHunkChanges.sort((a, b) => a.oldStart - b.oldStart);

    // Process the file line by line
    let oldIdx = 0;  // 0-based index into oldLines
    let newIdx = 0;  // 0-based index into newLines
    let hunkIdx = 0;

    while (oldIdx < oldLines.length || newIdx < newLines.length || hunkIdx < allHunkChanges.length) {
        // Check if we're at a hunk boundary
        const currentHunk = hunkIdx < allHunkChanges.length ? allHunkChanges[hunkIdx] : null;

        if (currentHunk && oldIdx + 1 === currentHunk.oldStart) {
            // Process this hunk
            let changeIdx = 0;
            while (changeIdx < currentHunk.changes.length) {
                const change = currentHunk.changes[changeIdx];

                if (change.type === 'context') {
                    aligned.push({
                        oldLine: oldLines[oldIdx],
                        newLine: newLines[newIdx],
                        oldLineNum: oldIdx + 1,
                        newLineNum: newIdx + 1,
                        changeType: 'unchanged'
                    });
                    oldIdx++;
                    newIdx++;
                    changeIdx++;
                } else if (change.type === 'remove') {
                    // Look ahead to see if next is an 'add' (modification)
                    if (changeIdx + 1 < currentHunk.changes.length &&
                        currentHunk.changes[changeIdx + 1].type === 'add') {
                        // Modified line
                        aligned.push({
                            oldLine: oldLines[oldIdx],
                            newLine: newLines[newIdx],
                            oldLineNum: oldIdx + 1,
                            newLineNum: newIdx + 1,
                            changeType: 'modified'
                        });
                        oldIdx++;
                        newIdx++;
                        changeIdx += 2;
                    } else {
                        // Pure removal
                        aligned.push({
                            oldLine: oldLines[oldIdx],
                            newLine: null,
                            oldLineNum: oldIdx + 1,
                            newLineNum: null,
                            changeType: 'removed'
                        });
                        oldIdx++;
                        changeIdx++;
                    }
                } else if (change.type === 'add') {
                    // Pure addition
                    aligned.push({
                        oldLine: null,
                        newLine: newLines[newIdx],
                        oldLineNum: null,
                        newLineNum: newIdx + 1,
                        changeType: 'added'
                    });
                    newIdx++;
                    changeIdx++;
                }
            }
            hunkIdx++;
        } else if (oldIdx < oldLines.length && newIdx < newLines.length) {
            // Not in a hunk - add unchanged line
            aligned.push({
                oldLine: oldLines[oldIdx],
                newLine: newLines[newIdx],
                oldLineNum: oldIdx + 1,
                newLineNum: newIdx + 1,
                changeType: 'unchanged'
            });
            oldIdx++;
            newIdx++;
        } else if (oldIdx < oldLines.length) {
            // Only old lines left (shouldn't happen normally)
            aligned.push({
                oldLine: oldLines[oldIdx],
                newLine: null,
                oldLineNum: oldIdx + 1,
                newLineNum: null,
                changeType: 'removed'
            });
            oldIdx++;
        } else if (newIdx < newLines.length) {
            // Only new lines left
            aligned.push({
                oldLine: null,
                newLine: newLines[newIdx],
                oldLineNum: null,
                newLineNum: newIdx + 1,
                changeType: 'added'
            });
            newIdx++;
        } else {
            break;
        }
    }

    return aligned;
}

interface HighlightTask {
    range: [number, number];
    fg: OverlayColorSpec;
    bg?: OverlayColorSpec;
    bold?: boolean;
    italic?: boolean;
    extend_to_line_end?: boolean;
}

/**
 * Generate virtual buffer content with diff highlighting for one side.
 * Returns entries, highlight tasks, and line byte offsets for scroll sync.
 */
function generateDiffPaneContent(
    alignedLines: AlignedLine[],
    side: 'old' | 'new'
): { entries: TextPropertyEntry[], highlights: HighlightTask[], lineByteOffsets: number[] } {
    const entries: TextPropertyEntry[] = [];
    const highlights: HighlightTask[] = [];
    const lineByteOffsets: number[] = [];
    let currentByte = 0;

    for (const line of alignedLines) {
        lineByteOffsets.push(currentByte);
        const content = side === 'old' ? line.oldLine : line.newLine;
        const lineNum = side === 'old' ? line.oldLineNum : line.newLineNum;
        const isFiller = content === null;

        // Format: "│ NNN │ content" or "│     │ ~~~~~~~~" for filler
        let lineNumStr: string;
        if (lineNum !== null) {
            lineNumStr = lineNum.toString().padStart(4, ' ');
        } else {
            lineNumStr = '    ';
        }

        // Gutter marker based on change type
        let gutterMarker = ' ';
        if (line.changeType === 'added' && side === 'new') gutterMarker = '+';
        else if (line.changeType === 'removed' && side === 'old') gutterMarker = '-';
        else if (line.changeType === 'modified') gutterMarker = '~';

        let lineText: string;
        if (isFiller) {
            // Filler line for alignment
            lineText = `│${gutterMarker}${lineNumStr} │ ${"░".repeat(40)}\n`;
        } else {
            lineText = `│${gutterMarker}${lineNumStr} │ ${content}\n`;
        }

        const lineLen = getByteLength(lineText);
        const prefixLen = getByteLength(`│${gutterMarker}${lineNumStr} │ `);

        entries.push({
            text: lineText,
            properties: {
                type: 'diff-line',
                changeType: line.changeType,
                lineNum: lineNum,
                side: side
            }
        });

        // Apply colors based on change type
        // Border color
        highlights.push({ range: [currentByte, currentByte + 1], fg: STYLE_BORDER });
        highlights.push({ range: [currentByte + prefixLen - 3, currentByte + prefixLen - 1], fg: STYLE_BORDER });

        // Line number color
        highlights.push({
            range: [currentByte + 2, currentByte + 6],
            fg: [120, 120, 120]  // Gray line numbers
        });

        if (isFiller) {
            // Filler styling - extend to full line width
            highlights.push({
                range: [currentByte + prefixLen, currentByte + lineLen - 1],
                fg: [60, 60, 60],
                bg: [30, 30, 30],
                extend_to_line_end: true
            });
        } else if (line.changeType === 'added' && side === 'new') {
            // Added line (green) - extend to full line width
            highlights.push({ range: [currentByte + 1, currentByte + 2], fg: STYLE_ADD_TEXT, bold: true }); // gutter marker
            highlights.push({
                range: [currentByte + prefixLen, currentByte + lineLen - 1],
                fg: STYLE_ADD_TEXT,
                bg: [30, 50, 30],
                extend_to_line_end: true
            });
        } else if (line.changeType === 'removed' && side === 'old') {
            // Removed line (red) - extend to full line width
            highlights.push({ range: [currentByte + 1, currentByte + 2], fg: STYLE_REMOVE_TEXT, bold: true }); // gutter marker
            highlights.push({
                range: [currentByte + prefixLen, currentByte + lineLen - 1],
                fg: STYLE_REMOVE_TEXT,
                bg: [50, 30, 30],
                extend_to_line_end: true
            });
        } else if (line.changeType === 'modified') {
            // Modified line - show word-level diff
            const oldText = line.oldLine || '';
            const newText = line.newLine || '';
            const diffParts = diffStrings(oldText, newText);

            let offset = currentByte + prefixLen;
            if (side === 'old') {
                highlights.push({ range: [currentByte + 1, currentByte + 2], fg: STYLE_REMOVE_TEXT, bold: true });
                // Highlight removed parts in old line
                for (const part of diffParts) {
                    const partLen = getByteLength(part.text);
                    if (part.type === 'removed') {
                        highlights.push({
                            range: [offset, offset + partLen],
                            fg: STYLE_REMOVE_TEXT,
                            bg: STYLE_REMOVE_BG,
                            bold: true
                        });
                    } else if (part.type === 'unchanged') {
                        highlights.push({
                            range: [offset, offset + partLen],
                            fg: STYLE_REMOVE_TEXT
                        });
                    }
                    if (part.type !== 'added') {
                        offset += partLen;
                    }
                }
            } else {
                highlights.push({ range: [currentByte + 1, currentByte + 2], fg: STYLE_ADD_TEXT, bold: true });
                // Highlight added parts in new line
                for (const part of diffParts) {
                    const partLen = getByteLength(part.text);
                    if (part.type === 'added') {
                        highlights.push({
                            range: [offset, offset + partLen],
                            fg: STYLE_ADD_TEXT,
                            bg: STYLE_ADD_BG,
                            bold: true
                        });
                    } else if (part.type === 'unchanged') {
                        highlights.push({
                            range: [offset, offset + partLen],
                            fg: STYLE_ADD_TEXT
                        });
                    }
                    if (part.type !== 'removed') {
                        offset += partLen;
                    }
                }
            }
        }

        currentByte += lineLen;
    }

    return { entries, highlights, lineByteOffsets };
}

// State for active side-by-side diff view
interface SideBySideDiffState {
    oldSplitId: number;
    newSplitId: number;
    oldBufferId: number;
    newBufferId: number;
    alignedLines: AlignedLine[];
    oldLineByteOffsets: number[];
    newLineByteOffsets: number[];
    scrollSyncGroupId: number | null;  // Core scroll sync group ID
}

let activeSideBySideState: SideBySideDiffState | null = null;
let nextScrollSyncGroupId = 1;

// State for composite buffer-based diff view
interface CompositeDiffState {
    compositeBufferId: number;
    oldBufferId: number;
    newBufferId: number;
    filePath: string;
}

let activeCompositeDiffState: CompositeDiffState | null = null;

async function review_drill_down() {
    // Use selected file from magit state instead of cursor properties
    if (state.files.length === 0) return;
    const selectedFile = state.files[state.selectedIndex];
    if (!selectedFile) return;

    // Create a minimal hunk-like reference for the rest of the function
    const h = { file: selectedFile.path, gitStatus: selectedFile.category };

    editor.setStatus(editor.t("status.loading_diff"));

    // Get all hunks for this file
    const fileHunks = state.hunks.filter(hunk => hunk.file === h.file);
    if (fileHunks.length === 0) return;

    // Get git root to construct absolute path
    const gitRootResult = await editor.spawnProcess("git", ["rev-parse", "--show-toplevel"]);
    if (gitRootResult.exit_code !== 0) {
        editor.setStatus(editor.t("status.not_git_repo"));
        return;
    }
    const gitRoot = gitRootResult.stdout.trim();
    const absoluteFilePath = editor.pathJoin(gitRoot, h.file);

    // Get old (HEAD) and new (working) file content
    let oldContent: string;
    const gitShow = await editor.spawnProcess("git", ["show", `HEAD:${h.file}`]);
    if (gitShow.exit_code !== 0) {
        oldContent = "";
    } else {
        oldContent = gitShow.stdout;
    }

    // Read new file content (use absolute path for readFile)
    const newContent = await editor.readFile(absoluteFilePath);
    if (newContent === null) {
        editor.setStatus(editor.t("status.failed_new_version"));
        return;
    }

    // Close any existing side-by-side views (old split-based approach)
    if (activeSideBySideState) {
        try {
            if (activeSideBySideState.scrollSyncGroupId !== null) {
                (editor as any).removeScrollSyncGroup(activeSideBySideState.scrollSyncGroupId);
            }
            editor.closeBuffer(activeSideBySideState.oldBufferId);
            editor.closeBuffer(activeSideBySideState.newBufferId);
        } catch {}
        activeSideBySideState = null;
    }

    // Close any existing composite diff view
    if (activeCompositeDiffState) {
        try {
            editor.closeCompositeBuffer(activeCompositeDiffState.compositeBufferId);
            editor.closeBuffer(activeCompositeDiffState.oldBufferId);
            editor.closeBuffer(activeCompositeDiffState.newBufferId);
        } catch {}
        activeCompositeDiffState = null;
    }

    // Create virtual buffers for old and new content
    const oldLines = oldContent.split('\n');
    const newLines = newContent.split('\n');

    const oldEntries: TextPropertyEntry[] = oldLines.map((line, idx) => ({
        text: line + '\n',
        properties: { type: 'line', lineNum: idx + 1 }
    }));

    const newEntries: TextPropertyEntry[] = newLines.map((line, idx) => ({
        text: line + '\n',
        properties: { type: 'line', lineNum: idx + 1 }
    }));

    // Create source buffers (hidden from tabs, used by composite)
    const oldResult = await editor.createVirtualBuffer({
        name: `*OLD:${h.file}*`,
        mode: "normal",
        readOnly: true,
        entries: oldEntries,
        showLineNumbers: true,
        editingDisabled: true,
        hiddenFromTabs: true
    });
    const oldBufferId = oldResult.bufferId;

    const newResult = await editor.createVirtualBuffer({
        name: `*NEW:${h.file}*`,
        mode: "normal",
        readOnly: true,
        entries: newEntries,
        showLineNumbers: true,
        editingDisabled: true,
        hiddenFromTabs: true
    });
    const newBufferId = newResult.bufferId;

    // Convert hunks to composite buffer format (parse counts from git diff)
    const compositeHunks: TsCompositeHunk[] = fileHunks.map(fh => {
        let oldCount = 0, newCount = 0;
        for (const line of fh.lines) {
            if (line.startsWith('-')) oldCount++;
            else if (line.startsWith('+')) newCount++;
            else if (line.startsWith(' ')) { oldCount++; newCount++; }
        }
        return {
            oldStart: Math.max(0, fh.oldRange.start - 1),
            oldCount: oldCount || 1,
            newStart: Math.max(0, fh.range.start - 1),
            newCount: newCount || 1
        };
    });

    // Create composite buffer with side-by-side layout
    const compositeBufferId = await editor.createCompositeBuffer({
        name: `*Diff: ${h.file}*`,
        mode: "diff-view",
        layout: {
            type: "side-by-side",
            ratios: [0.5, 0.5],
            showSeparator: true
        },
        sources: [
            {
                bufferId: oldBufferId,
                label: "OLD (HEAD)  [n/] next  [p/[] prev  [q] close",
                editable: false,
                style: {
                    gutterStyle: "diff-markers"
                }
            },
            {
                bufferId: newBufferId,
                label: "NEW (Working)",
                editable: false,
                style: {
                    gutterStyle: "diff-markers"
                }
            }
        ],
        hunks: compositeHunks.length > 0 ? compositeHunks : null,
        initialFocusHunk: compositeHunks.length > 0 ? 0 : undefined
    });

    // Store state for cleanup
    activeCompositeDiffState = {
        compositeBufferId,
        oldBufferId,
        newBufferId,
        filePath: h.file
    };

    // Show the composite buffer (replaces the review diff buffer)
    editor.showBuffer(compositeBufferId);

    const addedCount = fileHunks.reduce((sum, fh) => {
        return sum + fh.lines.filter(l => l.startsWith('+')).length;
    }, 0);
    const removedCount = fileHunks.reduce((sum, fh) => {
        return sum + fh.lines.filter(l => l.startsWith('-')).length;
    }, 0);
    const modifiedCount = Math.min(addedCount, removedCount);

    editor.setStatus(editor.t("status.diff_summary", { added: String(addedCount), removed: String(removedCount), modified: String(modifiedCount) }));
}
registerHandler("review_drill_down", review_drill_down);

// --- Hunk navigation for side-by-side diff view ---

function review_next_hunk() {
    if (!activeCompositeDiffState) return;
    editor.compositeNextHunk(activeCompositeDiffState.compositeBufferId);
}
registerHandler("review_next_hunk", review_next_hunk);

function review_prev_hunk() {
    if (!activeCompositeDiffState) return;
    editor.compositePrevHunk(activeCompositeDiffState.compositeBufferId);
}
registerHandler("review_prev_hunk", review_prev_hunk);

// Define the diff-view mode - inherits from "normal" for all standard navigation/selection/copy
// Only adds diff-specific keybindings (close, hunk navigation)
editor.defineMode("diff-view", [
    // Close the diff view
    ["q", "close"],
    // Hunk navigation (diff-specific)
    ["n", "review_next_hunk"],
    ["p", "review_prev_hunk"],
    ["]", "review_next_hunk"],
    ["[", "review_prev_hunk"],
], true);

// --- Review Comment Actions ---

function getCurrentHunkId(): string | null {
    // In magit mode, get the first hunk of the selected file
    if (state.files.length === 0) return null;
    const selectedFile = state.files[state.selectedIndex];
    if (!selectedFile) return null;
    const hunk = state.hunks.find(
        h => h.file === selectedFile.path && h.gitStatus === selectedFile.category
    );
    return hunk?.id || null;
}

interface PendingCommentInfo {
    hunkId: string;
    file: string;
    lineType?: 'add' | 'remove' | 'context';
    oldLine?: number;
    newLine?: number;
    lineContent?: string;
}

function getCurrentLineInfo(): PendingCommentInfo | null {
    // In magit mode, get line-level info from the selected diff line
    if (state.files.length === 0) return null;
    const selectedFile = state.files[state.selectedIndex];
    if (!selectedFile) return null;

    // Build diff lines and find the one at the current selection
    const leftWidth = Math.max(28, Math.floor(state.viewportWidth * 0.3));
    const rightWidth = state.viewportWidth - leftWidth - 1;
    const diffLines = buildDiffLines(rightWidth);
    if (diffLines.length === 0) return null;

    const idx = Math.min(state.diffSelectedLine, diffLines.length - 1);
    const line = diffLines[idx];
    if (!line || !line.hunkId) {
        // Fallback: find first hunk for this file
        const hunk = state.hunks.find(
            h => h.file === selectedFile.path && h.gitStatus === selectedFile.category
        );
        if (!hunk) return null;
        return { hunkId: hunk.id, file: hunk.file };
    }

    return {
        hunkId: line.hunkId,
        file: line.file || selectedFile.path,
        lineType: line.lineType,
        oldLine: line.oldLine,
        newLine: line.newLine,
        lineContent: line.lineContent
    };
}

// Pending prompt state for event-based prompt handling
let pendingCommentInfo: PendingCommentInfo | null = null;

async function review_add_comment() {
    const info = getCurrentLineInfo();
    if (!info) {
        editor.setStatus(editor.t("status.no_hunk_selected"));
        return;
    }
    pendingCommentInfo = info;

    // Show line context in prompt (if on a specific line)
    let lineRef = 'hunk';
    if (info.lineType === 'add' && info.newLine) {
        lineRef = `+${info.newLine}`;
    } else if (info.lineType === 'remove' && info.oldLine) {
        lineRef = `-${info.oldLine}`;
    } else if (info.newLine) {
        lineRef = `L${info.newLine}`;
    } else if (info.oldLine) {
        lineRef = `L${info.oldLine}`;
    }
    editor.startPrompt(editor.t("prompt.comment", { line: lineRef }), "review-comment");
}
registerHandler("review_add_comment", review_add_comment);

// Prompt event handlers
function on_review_prompt_confirm(args: { prompt_type: string; input: string }): boolean {
    if (args.prompt_type !== "review-comment") {
        return true; // Not our prompt
    }
    if (pendingCommentInfo && args.input && args.input.trim()) {
        const comment: ReviewComment = {
            id: `comment-${Date.now()}`,
            hunk_id: pendingCommentInfo.hunkId,
            file: pendingCommentInfo.file,
            text: args.input.trim(),
            timestamp: new Date().toISOString(),
            old_line: pendingCommentInfo.oldLine,
            new_line: pendingCommentInfo.newLine,
            line_content: pendingCommentInfo.lineContent,
            line_type: pendingCommentInfo.lineType
        };
        state.comments.push(comment);
        updateMagitDisplay();
        let lineRef = 'hunk';
        if (comment.line_type === 'add' && comment.new_line) {
            lineRef = `line +${comment.new_line}`;
        } else if (comment.line_type === 'remove' && comment.old_line) {
            lineRef = `line -${comment.old_line}`;
        } else if (comment.new_line) {
            lineRef = `line ${comment.new_line}`;
        } else if (comment.old_line) {
            lineRef = `line ${comment.old_line}`;
        }
        editor.setStatus(editor.t("status.comment_added", { line: lineRef }));
    }
    pendingCommentInfo = null;
    return true;
}
registerHandler("on_review_prompt_confirm", on_review_prompt_confirm);

function on_review_prompt_cancel(args: { prompt_type: string }): boolean {
    if (args.prompt_type === "review-comment") {
        pendingCommentInfo = null;
        editor.setStatus(editor.t("status.comment_cancelled"));
    }
    return true;
}
registerHandler("on_review_prompt_cancel", on_review_prompt_cancel);

// Register prompt event handlers
editor.on("prompt_confirmed", "on_review_prompt_confirm");
editor.on("prompt_confirmed", "on_review_discard_confirm");
editor.on("prompt_cancelled", "on_review_prompt_cancel");

async function review_approve_hunk() {
    const hunkId = getCurrentHunkId();
    if (!hunkId) return;
    const h = state.hunks.find(x => x.id === hunkId);
    if (h) {
        h.reviewStatus = 'approved';
        updateMagitDisplay();
        editor.setStatus(editor.t("status.hunk_approved"));
    }
}
registerHandler("review_approve_hunk", review_approve_hunk);

async function review_reject_hunk() {
    const hunkId = getCurrentHunkId();
    if (!hunkId) return;
    const h = state.hunks.find(x => x.id === hunkId);
    if (h) {
        h.reviewStatus = 'rejected';
        updateMagitDisplay();
        editor.setStatus(editor.t("status.hunk_rejected"));
    }
}
registerHandler("review_reject_hunk", review_reject_hunk);

async function review_needs_changes() {
    const hunkId = getCurrentHunkId();
    if (!hunkId) return;
    const h = state.hunks.find(x => x.id === hunkId);
    if (h) {
        h.reviewStatus = 'needs_changes';
        updateMagitDisplay();
        editor.setStatus(editor.t("status.hunk_needs_changes"));
    }
}
registerHandler("review_needs_changes", review_needs_changes);

async function review_question_hunk() {
    const hunkId = getCurrentHunkId();
    if (!hunkId) return;
    const h = state.hunks.find(x => x.id === hunkId);
    if (h) {
        h.reviewStatus = 'question';
        updateMagitDisplay();
        editor.setStatus(editor.t("status.hunk_question"));
    }
}
registerHandler("review_question_hunk", review_question_hunk);

async function review_clear_status() {
    const hunkId = getCurrentHunkId();
    if (!hunkId) return;
    const h = state.hunks.find(x => x.id === hunkId);
    if (h) {
        h.reviewStatus = 'pending';
        updateMagitDisplay();
        editor.setStatus(editor.t("status.hunk_status_cleared"));
    }
}
registerHandler("review_clear_status", review_clear_status);

async function review_set_overall_feedback() {
    const text = await editor.prompt(editor.t("prompt.overall_feedback"), state.overallFeedback || "");
    if (text !== null) {
        state.overallFeedback = text.trim();
        editor.setStatus(text.trim() ? editor.t("status.feedback_set") : editor.t("status.feedback_cleared"));
    }
}
registerHandler("review_set_overall_feedback", review_set_overall_feedback);

async function review_export_session() {
    const cwd = editor.getCwd();
    const reviewDir = editor.pathJoin(cwd, ".review");

    // Generate markdown content (writeFile creates parent directories)
    let md = `# Code Review Session\n`;
    md += `Date: ${new Date().toISOString()}\n\n`;

    if (state.originalRequest) {
        md += `## Original Request\n${state.originalRequest}\n\n`;
    }

    if (state.overallFeedback) {
        md += `## Overall Feedback\n${state.overallFeedback}\n\n`;
    }

    // Stats
    const approved = state.hunks.filter(h => h.reviewStatus === 'approved').length;
    const rejected = state.hunks.filter(h => h.reviewStatus === 'rejected').length;
    const needsChanges = state.hunks.filter(h => h.reviewStatus === 'needs_changes').length;
    const questions = state.hunks.filter(h => h.reviewStatus === 'question').length;
    md += `## Summary\n`;
    md += `- Total hunks: ${state.hunks.length}\n`;
    md += `- Approved: ${approved}\n`;
    md += `- Rejected: ${rejected}\n`;
    md += `- Needs changes: ${needsChanges}\n`;
    md += `- Questions: ${questions}\n\n`;

    // Group by file
    const fileGroups: Record<string, Hunk[]> = {};
    for (const hunk of state.hunks) {
        if (!fileGroups[hunk.file]) fileGroups[hunk.file] = [];
        fileGroups[hunk.file].push(hunk);
    }

    for (const [file, hunks] of Object.entries(fileGroups)) {
        md += `## File: ${file}\n\n`;
        for (const hunk of hunks) {
            const statusStr = hunk.reviewStatus.toUpperCase();
            md += `### ${hunk.contextHeader || 'Hunk'} (line ${hunk.range.start})\n`;
            md += `**Status**: ${statusStr}\n\n`;

            const hunkComments = state.comments.filter(c => c.hunk_id === hunk.id);
            if (hunkComments.length > 0) {
                md += `**Comments:**\n`;
                for (const c of hunkComments) {
                    // Format line reference
                    let lineRef = '';
                    if (c.line_type === 'add' && c.new_line) {
                        lineRef = `[+${c.new_line}]`;
                    } else if (c.line_type === 'remove' && c.old_line) {
                        lineRef = `[-${c.old_line}]`;
                    } else if (c.new_line) {
                        lineRef = `[L${c.new_line}]`;
                    } else if (c.old_line) {
                        lineRef = `[L${c.old_line}]`;
                    }
                    md += `> 💬 ${lineRef} ${c.text}\n`;
                    if (c.line_content) {
                        md += `> \`${c.line_content.trim()}\`\n`;
                    }
                    md += `\n`;
                }
            }
        }
    }

    // Write file
    const filePath = editor.pathJoin(reviewDir, "session.md");
    await editor.writeFile(filePath, md);
    editor.setStatus(editor.t("status.exported", { path: filePath }));
}
registerHandler("review_export_session", review_export_session);

async function review_export_json() {
    const cwd = editor.getCwd();
    const reviewDir = editor.pathJoin(cwd, ".review");
    // writeFile creates parent directories

    const session = {
        version: "1.0",
        timestamp: new Date().toISOString(),
        original_request: state.originalRequest || null,
        overall_feedback: state.overallFeedback || null,
        files: {} as Record<string, any>
    };

    for (const hunk of state.hunks) {
        if (!session.files[hunk.file]) session.files[hunk.file] = { hunks: [] };
        const hunkComments = state.comments.filter(c => c.hunk_id === hunk.id);
        session.files[hunk.file].hunks.push({
            context: hunk.contextHeader,
            old_lines: [hunk.oldRange.start, hunk.oldRange.end],
            new_lines: [hunk.range.start, hunk.range.end],
            status: hunk.reviewStatus,
            comments: hunkComments.map(c => ({
                text: c.text,
                line_type: c.line_type || null,
                old_line: c.old_line || null,
                new_line: c.new_line || null,
                line_content: c.line_content || null
            }))
        });
    }

    const filePath = editor.pathJoin(reviewDir, "session.json");
    await editor.writeFile(filePath, JSON.stringify(session, null, 2));
    editor.setStatus(editor.t("status.exported", { path: filePath }));
}
registerHandler("review_export_json", review_export_json);

async function start_review_diff() {
    editor.setStatus(editor.t("status.generating"));
    editor.setContext("review-mode", true);

    // Get viewport size
    const viewport = editor.getViewport();
    if (viewport) {
        state.viewportWidth = viewport.width;
        state.viewportHeight = viewport.height;
    }

    // Fetch data using new git status approach
    state.files = await getGitStatus();
    state.hunks = await fetchDiffsForFiles(state.files);
    state.comments = [];
    state.selectedIndex = 0;
    state.fileScrollOffset = 0;
    state.diffScrollOffset = 0;
    state.diffSelectedLine = 0;
    state.focusPanel = 'files';

    // Build initial display
    const initialEntries = buildMagitDisplayEntries();

    const bufferId = await VirtualBufferFactory.create({
        name: "*Review Diff*", mode: "review-mode", readOnly: true,
        entries: initialEntries, showLineNumbers: false, showCursors: false
    });
    state.reviewBufferId = bufferId;

    // Register resize handler
    editor.on("resize", "onReviewDiffResize");

    editor.setStatus(editor.t("status.review_summary", { count: String(state.hunks.length) }));
    editor.on("buffer_activated", "on_review_buffer_activated");
    editor.on("buffer_closed", "on_review_buffer_closed");
}
registerHandler("start_review_diff", start_review_diff);

function stop_review_diff() {
    state.reviewBufferId = null;
    editor.setContext("review-mode", false);
    editor.off("resize", "onReviewDiffResize");
    editor.off("buffer_activated", "on_review_buffer_activated");
    editor.off("buffer_closed", "on_review_buffer_closed");
    editor.setStatus(editor.t("status.stopped"));
}
registerHandler("stop_review_diff", stop_review_diff);


function on_review_buffer_activated(data: any) {
    if (data.buffer_id === state.reviewBufferId) refreshMagitData();
}
registerHandler("on_review_buffer_activated", on_review_buffer_activated);

function on_review_buffer_closed(data: any) {
    if (data.buffer_id === state.reviewBufferId) stop_review_diff();
}
registerHandler("on_review_buffer_closed", on_review_buffer_closed);

// Side-by-side diff for current file using composite buffers
async function side_by_side_diff_current_file() {
    const bid = editor.getActiveBufferId();
    const absolutePath = editor.getBufferPath(bid);

    if (!absolutePath) {
        editor.setStatus(editor.t("status.no_file_open"));
        return;
    }

    editor.setStatus(editor.t("status.loading_diff"));

    // Get the file's directory and name for running git commands
    const fileDir = editor.pathDirname(absolutePath);
    const fileName = editor.pathBasename(absolutePath);

    // Run git commands from the file's directory to avoid path format issues on Windows
    const gitRootResult = await editor.spawnProcess("git", ["-C", fileDir, "rev-parse", "--show-toplevel"]);
    if (gitRootResult.exit_code !== 0) {
        editor.setStatus(editor.t("status.not_git_repo"));
        return;
    }
    const gitRoot = gitRootResult.stdout.trim();

    // Get relative path from git root using git itself (handles Windows paths correctly)
    const relPathResult = await editor.spawnProcess("git", ["-C", fileDir, "ls-files", "--full-name", fileName]);
    let filePath: string;
    if (relPathResult.exit_code === 0 && relPathResult.stdout.trim()) {
        filePath = relPathResult.stdout.trim();
    } else {
        // File might be untracked, compute relative path manually
        // Normalize paths: replace backslashes with forward slashes for comparison
        const normAbsPath = absolutePath.replace(/\\/g, '/');
        const normGitRoot = gitRoot.replace(/\\/g, '/');
        if (normAbsPath.toLowerCase().startsWith(normGitRoot.toLowerCase())) {
            filePath = normAbsPath.substring(normGitRoot.length + 1);
        } else {
            // Fallback to just the filename
            filePath = fileName;
        }
    }

    // Check if the file is untracked
    const isTrackedResult = await editor.spawnProcess("git", ["-C", gitRoot, "ls-files", "--", filePath]);
    const isUntracked = isTrackedResult.exit_code !== 0 || !isTrackedResult.stdout.trim();

    // Get hunks for this specific file
    let diffOutput: string;
    if (isUntracked) {
        // For untracked files, use --no-index to diff against /dev/null
        const result = await editor.spawnProcess("git", ["-C", gitRoot, "diff", "--no-index", "--unified=3", "--", "/dev/null", filePath]);
        // git diff --no-index exits with 1 when there are differences, which is expected
        diffOutput = result.stdout || "";
    } else {
        // For tracked files, use normal diff against HEAD
        const result = await editor.spawnProcess("git", ["-C", gitRoot, "diff", "HEAD", "--unified=3", "--", filePath]);
        if (result.exit_code !== 0) {
            editor.setStatus(editor.t("status.failed_git_diff"));
            return;
        }
        diffOutput = result.stdout;
    }

    // Parse hunks from diff output
    const lines = diffOutput.split('\n');
    const fileHunks: Hunk[] = [];
    let currentHunk: Hunk | null = null;

    for (const line of lines) {
        if (line.startsWith('@@')) {
            const match = line.match(/@@ -(\d+),?(\d*) \+(\d+),?(\d*) @@(.*)/);
            if (match) {
                const oldStart = parseInt(match[1]);
                const oldCount = match[2] ? parseInt(match[2]) : 1;
                const newStart = parseInt(match[3]);
                const newCount = match[4] ? parseInt(match[4]) : 1;
                currentHunk = {
                    id: `${filePath}:${newStart}`,
                    file: filePath,
                    range: { start: newStart, end: newStart + newCount - 1 },
                    oldRange: { start: oldStart, end: oldStart + oldCount - 1 },
                    type: isUntracked ? 'add' : 'modify',
                    lines: [],
                    status: 'pending',
                    reviewStatus: 'pending',
                    contextHeader: match[5]?.trim() || "",
                    byteOffset: 0
                };
                fileHunks.push(currentHunk);
            }
        } else if (currentHunk && (line.startsWith('+') || line.startsWith('-') || line.startsWith(' '))) {
            if (!line.startsWith('---') && !line.startsWith('+++')) {
                currentHunk.lines.push(line);
            }
        }
    }

    if (fileHunks.length === 0) {
        editor.setStatus(editor.t("status.no_changes"));
        return;
    }

    // Get old (HEAD) and new (working) file content (use -C gitRoot since filePath is relative to git root)
    let oldContent: string;
    if (isUntracked) {
        // For untracked files, old content is empty (file didn't exist before)
        oldContent = "";
    } else {
        const gitShow = await editor.spawnProcess("git", ["-C", gitRoot, "show", `HEAD:${filePath}`]);
        if (gitShow.exit_code !== 0) {
            editor.setStatus(editor.t("status.failed_old_new_file"));
            return;
        }
        oldContent = gitShow.stdout;
    }

    // Read new file content (use absolute path for readFile)
    const newContent = await editor.readFile(absolutePath);
    if (newContent === null) {
        editor.setStatus(editor.t("status.failed_new_version"));
        return;
    }

    // Close any existing side-by-side views
    if (activeSideBySideState) {
        try {
            if (activeSideBySideState.scrollSyncGroupId !== null) {
                (editor as any).removeScrollSyncGroup(activeSideBySideState.scrollSyncGroupId);
            }
            editor.closeBuffer(activeSideBySideState.oldBufferId);
            editor.closeBuffer(activeSideBySideState.newBufferId);
        } catch {}
        activeSideBySideState = null;
    }

    // Close any existing composite diff view
    if (activeCompositeDiffState) {
        try {
            editor.closeCompositeBuffer(activeCompositeDiffState.compositeBufferId);
            editor.closeBuffer(activeCompositeDiffState.oldBufferId);
            editor.closeBuffer(activeCompositeDiffState.newBufferId);
        } catch {}
        activeCompositeDiffState = null;
    }

    // Create virtual buffers for old and new content
    const oldLines = oldContent.split('\n');
    const newLines = newContent.split('\n');

    const oldEntries: TextPropertyEntry[] = oldLines.map((line, idx) => ({
        text: line + '\n',
        properties: { type: 'line', lineNum: idx + 1 }
    }));

    const newEntries: TextPropertyEntry[] = newLines.map((line, idx) => ({
        text: line + '\n',
        properties: { type: 'line', lineNum: idx + 1 }
    }));

    // Create source buffers (hidden from tabs, used by composite)
    const oldResult = await editor.createVirtualBuffer({
        name: `*OLD:${filePath}*`,
        mode: "normal",
        readOnly: true,
        entries: oldEntries,
        showLineNumbers: true,
        editingDisabled: true,
        hiddenFromTabs: true
    });
    const oldBufferId = oldResult.bufferId;

    const newResult = await editor.createVirtualBuffer({
        name: `*NEW:${filePath}*`,
        mode: "normal",
        readOnly: true,
        entries: newEntries,
        showLineNumbers: true,
        editingDisabled: true,
        hiddenFromTabs: true
    });
    const newBufferId = newResult.bufferId;

    // Convert hunks to composite buffer format
    const compositeHunks: TsCompositeHunk[] = fileHunks.map(h => ({
        oldStart: Math.max(0, h.oldRange.start - 1),  // Convert to 0-indexed (0 for new files)
        oldCount: Math.max(1, h.oldRange.end - h.oldRange.start + 1),
        newStart: Math.max(0, h.range.start - 1),     // Convert to 0-indexed
        newCount: h.range.end - h.range.start + 1
    }));

    // Create composite buffer with side-by-side layout
    const compositeBufferId = await editor.createCompositeBuffer({
        name: `*Diff: ${filePath}*`,
        mode: "diff-view",
        layout: {
            type: "side-by-side",
            ratios: [0.5, 0.5],
            showSeparator: true
        },
        sources: [
            {
                bufferId: oldBufferId,
                label: "OLD (HEAD)  [n/] next  [p/[] prev  [q] close",
                editable: false,
                style: {
                    gutterStyle: "diff-markers"
                }
            },
            {
                bufferId: newBufferId,
                label: "NEW (Working)",
                editable: false,
                style: {
                    gutterStyle: "diff-markers"
                }
            }
        ],
        hunks: compositeHunks.length > 0 ? compositeHunks : null
    });

    // Store state for cleanup
    activeCompositeDiffState = {
        compositeBufferId,
        oldBufferId,
        newBufferId,
        filePath
    };

    // Show the composite buffer
    editor.showBuffer(compositeBufferId);

    const addedCount = fileHunks.reduce((sum, h) => {
        return sum + h.lines.filter(l => l.startsWith('+')).length;
    }, 0);
    const removedCount = fileHunks.reduce((sum, h) => {
        return sum + h.lines.filter(l => l.startsWith('-')).length;
    }, 0);
    const modifiedCount = Math.min(addedCount, removedCount);

    editor.setStatus(editor.t("status.diff_summary", { added: String(addedCount), removed: String(removedCount), modified: String(modifiedCount) }));
}
registerHandler("side_by_side_diff_current_file", side_by_side_diff_current_file);

// Register Modes and Commands
editor.registerCommand("%cmd.review_diff", "%cmd.review_diff_desc", "start_review_diff", null);
editor.registerCommand("%cmd.stop_review_diff", "%cmd.stop_review_diff_desc", "stop_review_diff", "review-mode");
editor.registerCommand("%cmd.refresh_review_diff", "%cmd.refresh_review_diff_desc", "review_refresh", "review-mode");
editor.registerCommand("%cmd.side_by_side_diff", "%cmd.side_by_side_diff_desc", "side_by_side_diff_current_file", null);

// Review Comment Commands
editor.registerCommand("%cmd.add_comment", "%cmd.add_comment_desc", "review_add_comment", "review-mode");
editor.registerCommand("%cmd.approve_hunk", "%cmd.approve_hunk_desc", "review_approve_hunk", "review-mode");
editor.registerCommand("%cmd.reject_hunk", "%cmd.reject_hunk_desc", "review_reject_hunk", "review-mode");
editor.registerCommand("%cmd.needs_changes", "%cmd.needs_changes_desc", "review_needs_changes", "review-mode");
editor.registerCommand("%cmd.question", "%cmd.question_desc", "review_question_hunk", "review-mode");
editor.registerCommand("%cmd.clear_status", "%cmd.clear_status_desc", "review_clear_status", "review-mode");
editor.registerCommand("%cmd.overall_feedback", "%cmd.overall_feedback_desc", "review_set_overall_feedback", "review-mode");
editor.registerCommand("%cmd.export_markdown", "%cmd.export_markdown_desc", "review_export_session", "review-mode");
editor.registerCommand("%cmd.export_json", "%cmd.export_json_desc", "review_export_json", "review-mode");

// Handler for when buffers are closed - cleans up scroll sync groups and composite buffers
function on_buffer_closed(data: any) {
    // If one of the diff view buffers is closed, clean up the scroll sync group
    if (activeSideBySideState) {
        if (data.buffer_id === activeSideBySideState.oldBufferId ||
            data.buffer_id === activeSideBySideState.newBufferId) {
            // Remove scroll sync group
            if (activeSideBySideState.scrollSyncGroupId !== null) {
                try {
                    (editor as any).removeScrollSyncGroup(activeSideBySideState.scrollSyncGroupId);
                } catch {}
            }
            activeSideBySideState = null;
            activeDiffViewState = null;
        }
    }

    // Clean up composite diff state if the composite buffer is closed
    if (activeCompositeDiffState) {
        if (data.buffer_id === activeCompositeDiffState.compositeBufferId) {
            // Close the source buffers
            try {
                editor.closeBuffer(activeCompositeDiffState.oldBufferId);
                editor.closeBuffer(activeCompositeDiffState.newBufferId);
            } catch {}
            activeCompositeDiffState = null;
        }
    }
}
registerHandler("on_buffer_closed", on_buffer_closed);

editor.on("buffer_closed", "on_buffer_closed");

editor.defineMode("review-mode", [
    // Navigation (arrow keys, vim keys, page keys)
    ["Up", "review_nav_up"], ["Down", "review_nav_down"],
    ["k", "review_nav_up"], ["j", "review_nav_down"],
    ["PageUp", "review_page_up"], ["PageDown", "review_page_down"],
    ["Home", "review_nav_home"], ["End", "review_nav_end"],
    ["Tab", "review_toggle_focus"],
    ["Left", "review_focus_files"], ["Right", "review_focus_diff"],
    // Drill-down
    ["Enter", "review_drill_down"],
    // Git actions (plain letter keys — safe because buffer is read-only with cursors hidden)
    ["s", "review_stage_file"], ["u", "review_unstage_file"],
    ["d", "review_discard_file"],
    ["r", "review_refresh"],
    // Review actions
    ["a", "review_approve_hunk"],
    ["c", "review_add_comment"],
    // Export
    ["e", "review_export_session"],
], true);

editor.debug("Review Diff plugin loaded with review comments support");
