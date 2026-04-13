/// <reference path="./lib/fresh.d.ts" />
/// <reference path="./lib/types.ts" />
/// <reference path="./lib/virtual-buffer-factory.ts" />

// Review Diff Plugin
// Magit-style split-panel UI for reviewing and staging code changes.
// Left panel: file list (staged/unstaged/untracked). Right panel: diff.
// Actions: stage/unstage/discard hunks or files, line comments, export.
const editor = getEditor();

import { createVirtualBufferFactory } from "./lib/virtual-buffer-factory.ts";
const VirtualBufferFactory = createVirtualBufferFactory(editor);



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
 *
 * Scrolling and cursor tracking inside the panel buffers is handled by the
 * editor core natively — this state only mirrors what the plugin needs to
 * know between events (selected file, focused panel, hunk header rows for
 * `n`/`p` jumps).
 */
/**
 * Why the file list is empty. `null` means `state.files` has entries; the
 * other two distinguish "cwd is not a git repo" from "repo is clean" so the
 * panels can show a specific message instead of rendering byte-identically.
 */
type EmptyStateReason = 'not_git' | 'clean' | null;

interface ReviewState {
  hunks: Hunk[];
  comments: ReviewComment[];
  note: string;
  reviewBufferId: number | null;
  // Files with changes (used for section grouping + headers in the
  // unified stream). Order matches the order they appear in the diff.
  files: FileEntry[];
  emptyState: EmptyStateReason;
  viewportWidth: number;
  viewportHeight: number;
  focusPanel: 'diff' | 'comments';
  groupId: number | null;
  panelBuffers: Record<string, number>;
  // Caches populated each time the unified diff stream is rebuilt —
  // used by `n`/`p` hunk navigation, to translate row numbers into byte
  // positions for `setBufferCursor`, and to draw the cursor-line
  // highlight overlay. `diffLineByteOffsets` has length `(rowCount + 1)`:
  // index `i` is the byte offset of row `i + 1`, and the final entry is
  // the total buffer length.
  hunkHeaderRows: number[];        // 1-indexed row numbers in the unified buffer
  diffLineByteOffsets: number[];
  diffCursorRow: number;           // 1-indexed, last known cursor row in diff buffer
  // Maps file key (`${path}\0${category}`) -> 1-indexed row of the
  // file-header row in the unified stream. Used by mouse/collapse/sticky.
  fileHeaderRows: Record<string, number>;
  // Files that are currently collapsed (`${path}\0${category}` keys).
  // Persists across refreshes within a session; cleared on start_review_diff.
  collapsedFiles: Set<string>;
  // Maps a 1-indexed row in the comments panel -> comment id
  commentsByRow: Record<number, string>;
  // Current selection in the comments panel (1-indexed row, 0 means none)
  commentsSelectedRow: number;
  // Sticky header current content (for Step 4)
  stickyCurrentFile: string | null;
  // Visual line-selection state. Active iff non-null. start and end are
  // 1-indexed rows in the unified stream; hunkId pins the selection to
  // a single hunk (selections that cross hunks are rejected).
  lineSelection: { startRow: number; endRow: number; hunkId: string } | null;
}

const state: ReviewState = {
  hunks: [],
  comments: [],
  note: '',
  reviewBufferId: null,
  files: [],
  emptyState: null,
  viewportWidth: 80,
  viewportHeight: 24,
  focusPanel: 'diff',
  groupId: null,
  panelBuffers: {},
  hunkHeaderRows: [],
  diffLineByteOffsets: [],
  diffCursorRow: 1,
  fileHeaderRows: {},
  collapsedFiles: new Set(),
  commentsByRow: {},
  commentsSelectedRow: 0,
  stickyCurrentFile: null,
  lineSelection: null,
};

function fileKey(f: FileEntry): string { return `${f.path}\0${f.category}`; }
function fileKeyOf(path: string, category: string): string { return `${path}\0${category}`; }

// Theme colour for the synthetic "cursor line" highlight in the panel
// buffers. Reintroduced after the per-line bg overlay was deleted from the
// builders — `applyCursorLineOverlay` writes it on every cursor_moved event.
const STYLE_SELECTED_BG: OverlayColorSpec = "editor.selection_bg";
const CURSOR_LINE_NS = "review-cursor-line";

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

const STYLE_SECTION_HEADER: OverlayColorSpec = "syntax.type";
const STYLE_COMMENT: OverlayColorSpec = "diagnostic.warning_fg";


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

/**
 * Inline word-level diff between two changed lines.
 *
 * Used to highlight the *changed region* inside a -/+ pair, called once per
 * adjacent pair while building a file's diff. The previous implementation
 * was a full O(n*m) LCS that allocated an (n+1)*(m+1) DP table per pair —
 * fast enough for short lines, but for files with hundreds of long-line
 * changes (e.g. `audit_mode.ts` itself) it added hundreds of milliseconds
 * to every diff rebuild and made file-list navigation visibly laggy.
 *
 * This O(n+m) scan finds the longest common prefix and suffix and reports
 * everything in between as the changed region. It misses internal matches
 * (e.g. it can't tell that "abc-xy-def" → "abc-zw-def" only changed the
 * middle "xy"), but for inline highlighting that's fine — the human eye is
 * already drawn to the line as a whole, the highlight just answers "where
 * inside the line did the change happen?". The cost difference is dramatic:
 * for two 200-char lines, ~400 char compares vs. ~40 000.
 */
function diffStrings(oldStr: string, newStr: string): DiffPart[] {
    const n = oldStr.length;
    const m = newStr.length;
    let pre = 0;
    const minLen = Math.min(n, m);
    while (pre < minLen && oldStr.charCodeAt(pre) === newStr.charCodeAt(pre)) pre++;
    let suf = 0;
    while (
        suf < n - pre &&
        suf < m - pre &&
        oldStr.charCodeAt(n - 1 - suf) === newStr.charCodeAt(m - 1 - suf)
    ) {
        suf++;
    }

    const parts: DiffPart[] = [];
    if (pre > 0) parts.push({ text: oldStr.slice(0, pre), type: 'unchanged' });
    if (pre < n - suf) parts.push({ text: oldStr.slice(pre, n - suf), type: 'removed' });
    if (pre < m - suf) parts.push({ text: newStr.slice(pre, m - suf), type: 'added' });
    if (suf > 0) parts.push({ text: oldStr.slice(n - suf), type: 'unchanged' });
    return parts;
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
 *
 * `emptyReason` distinguishes the two no-content cases so the UI can explain
 * itself instead of rendering a blank pane:
 *   - `'not_git'`: `git status` failed (no repo at cwd).
 *   - `'clean'`: `git status` succeeded but returned no entries.
 *   - `null`: files were found; render them normally.
 */
interface GitStatusResult {
    files: FileEntry[];
    emptyReason: EmptyStateReason;
}

async function getGitStatus(): Promise<GitStatusResult> {
    const result = await editor.spawnProcess("git", ["status", "--porcelain", "-z"]);
    if (result.exit_code !== 0) {
        return { files: [], emptyReason: 'not_git' };
    }
    const files = parseGitStatusPorcelain(result.stdout);
    return {
        files,
        emptyReason: files.length === 0 ? 'clean' : null,
    };
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
    type: 'hunk-header' | 'add' | 'remove' | 'context' | 'empty' | 'comment' | 'file-header' | 'section-header';
    filePath?: string;   // for file-header rows
    fileKey?: string;    // for file-header rows
    fileIndex?: number;  // for file-header rows
    style?: Partial<OverlayOptions>;
    inlineOverlays?: InlineOverlay[];
    // Line metadata for comment attachment
    hunkId?: string;
    file?: string;
    lineType?: 'add' | 'remove' | 'context';
    oldLine?: number;
    newLine?: number;
    lineContent?: string;
    commentId?: string;
}

/**
 * Compute +N / -M line counts for a file.
 */
function fileChangeCounts(file: FileEntry): { added: number; removed: number } {
    let added = 0;
    let removed = 0;
    for (const h of state.hunks) {
        if (h.file === file.path && h.gitStatus === file.category) {
            for (const line of h.lines) {
                if (line[0] === '+') added++;
                else if (line[0] === '-') removed++;
            }
        }
    }
    return { added, removed };
}

/**
 * Push inline comment lines for a given diff line into the lines array.
 */
function pushLineComments(
    lines: DiffLine[], hunk: Hunk,
    lineType: 'add' | 'remove' | 'context',
    oldLine: number | undefined, newLine: number | undefined
) {
    const lineComments = state.comments.filter(c =>
        c.hunk_id === hunk.id && (
            (c.line_type === 'add' && c.new_line === newLine) ||
            (c.line_type === 'remove' && c.old_line === oldLine) ||
            (c.line_type === 'context' && c.new_line === newLine)
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
            commentId: comment.id,
            style: { fg: STYLE_COMMENT, italic: true },
        });
    }
}

/**
 * Build the diff lines for the unified stream.
 * Emits one file-header row per file, followed by its hunks inline.
 * When the file is collapsed, only the header is emitted.
 */
function buildDiffLines(_rightWidth: number): DiffLine[] {
    const lines: DiffLine[] = [];
    if (state.files.length === 0) {
        if (state.emptyState === 'not_git') {
            lines.push({
                text: editor.t("status.not_git_repo") || "Not a git repository",
                type: 'empty',
                style: { fg: STYLE_SECTION_HEADER, italic: true },
            });
        } else if (state.emptyState === 'clean') {
            lines.push({
                text: editor.t("panel.no_changes") || "No changes to review.",
                type: 'empty',
                style: { fg: STYLE_SECTION_HEADER, italic: true },
            });
        }
        return lines;
    }

    let lastCategory: string | undefined;
    for (let fi = 0; fi < state.files.length; fi++) {
        const file = state.files[fi];

        // Section divider when the category changes
        if (file.category !== lastCategory) {
            lastCategory = file.category;
            let label: string = file.category;
            if (file.category === 'staged') label = editor.t("section.staged") || "Staged";
            else if (file.category === 'unstaged') label = editor.t("section.unstaged") || "Changes";
            else if (file.category === 'untracked') label = editor.t("section.untracked") || "Untracked";
            lines.push({
                text: `══ ${label} ══`,
                type: 'section-header',
                style: { fg: STYLE_SECTION_HEADER, bold: true },
            });
        }

        // File header line
        const counts = fileChangeCounts(file);
        const key = fileKey(file);
        const collapsed = state.collapsedFiles.has(key);
        const triangle = collapsed ? '▸' : '▾';
        const filename = file.origPath ? `${file.origPath} → ${file.path}` : file.path;
        const headerText = `${triangle} ${filename}   +${counts.added} / -${counts.removed}`;
        lines.push({
            text: headerText,
            type: 'file-header',
            file: file.path,
            filePath: file.path,
            fileKey: key,
            fileIndex: fi,
            style: { fg: STYLE_HEADER, bold: true },
        });

        // If collapsed, just emit a blank separator and skip hunks
        if (collapsed) {
            lines.push({ text: '', type: 'empty' });
            continue;
        }

        // Find hunks for this file
        const fileHunks = state.hunks.filter(
            h => h.file === file.path && h.gitStatus === file.category
        );

        if (fileHunks.length === 0) {
            if (file.status === 'R' && file.origPath) {
                lines.push({ text: `  Renamed from ${file.origPath}`, type: 'empty', style: { fg: STYLE_SECTION_HEADER } });
            } else if (file.status === 'D') {
                lines.push({ text: "  (file deleted)", type: 'empty' });
            } else if (file.status === 'T') {
                lines.push({ text: "  (type change: file ↔ symlink)", type: 'empty', style: { fg: STYLE_SECTION_HEADER } });
            } else if (file.status === '?' && file.path.endsWith('/')) {
                lines.push({ text: "  (untracked directory)", type: 'empty' });
            } else {
                lines.push({ text: "  (no diff available)", type: 'empty' });
            }
            lines.push({ text: '', type: 'empty' });
            continue;
        }

        for (const hunk of fileHunks) {
        // Hunk header with review status indicator
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

        // Render hunk-level comments (those with no line_type) right
        // after the hunk header so they are visible in the diff view.
        const hunkComments = state.comments.filter(c =>
            c.hunk_id === hunk.id && !c.line_type
        );
        for (const comment of hunkComments) {
            lines.push({
                text: `  \u00bb [hunk] ${comment.text}`,
                type: 'comment',
                commentId: comment.id,
                style: { fg: STYLE_COMMENT, italic: true },
            });
        }

        // Track actual file line numbers as we iterate
        let oldLineNum = hunk.oldRange.start;
        let newLineNum = hunk.range.start;

        // Diff content lines with word-level highlighting for adjacent -/+ pairs
        for (let li = 0; li < hunk.lines.length; li++) {
            const line = hunk.lines[li];
            const nextLine = hunk.lines[li + 1];
            const prefix = line[0];
            const lineType: 'add' | 'remove' | 'context' =
                prefix === '+' ? 'add' : prefix === '-' ? 'remove' : 'context';
            const curOldLine = lineType !== 'add' ? oldLineNum : undefined;
            const curNewLine = lineType !== 'remove' ? newLineNum : undefined;

            // Detect adjacent -/+ pair for word-level diff
            if (prefix === '-' && nextLine && nextLine[0] === '+') {
                const oldContent = line.substring(1);
                const newContent = nextLine.substring(1);
                const parts = diffStrings(oldContent, newContent);

                // Build inline overlays for removed line
                const removeOverlays: InlineOverlay[] = [];
                let rOffset = getByteLength(line[0]); // skip prefix
                for (const part of parts) {
                    const pLen = getByteLength(part.text);
                    if (part.type === 'removed') {
                        removeOverlays.push({ start: rOffset, end: rOffset + pLen, style: { fg: STYLE_REMOVE_TEXT, bg: STYLE_REMOVE_BG, bold: true } });
                    }
                    if (part.type !== 'added') rOffset += pLen;
                }
                lines.push({
                    text: line, type: 'remove',
                    style: { bg: STYLE_REMOVE_BG, extendToLineEnd: true },
                    hunkId: hunk.id, file: hunk.file,
                    lineType: 'remove', oldLine: curOldLine, newLine: undefined, lineContent: line,
                    inlineOverlays: removeOverlays.length > 0 ? removeOverlays : undefined,
                });
                // Inline comments for the removed line
                pushLineComments(lines, hunk, 'remove', curOldLine, undefined);
                oldLineNum++;

                // Build inline overlays for added line
                const addOverlays: InlineOverlay[] = [];
                let aOffset = getByteLength(nextLine[0]);
                for (const part of parts) {
                    const pLen = getByteLength(part.text);
                    if (part.type === 'added') {
                        addOverlays.push({ start: aOffset, end: aOffset + pLen, style: { fg: STYLE_ADD_TEXT, bg: STYLE_ADD_BG, bold: true } });
                    }
                    if (part.type !== 'removed') aOffset += pLen;
                }
                lines.push({
                    text: nextLine, type: 'add',
                    style: { bg: STYLE_ADD_BG, extendToLineEnd: true },
                    hunkId: hunk.id, file: hunk.file,
                    lineType: 'add', oldLine: undefined, newLine: newLineNum, lineContent: nextLine,
                    inlineOverlays: addOverlays.length > 0 ? addOverlays : undefined,
                });
                pushLineComments(lines, hunk, 'add', undefined, newLineNum);
                newLineNum++;
                li++; // skip the + line we already processed
                continue;
            }

            if (prefix === '+') {
                lines.push({
                    text: line, type: 'add',
                    style: { bg: STYLE_ADD_BG, extendToLineEnd: true },
                    hunkId: hunk.id, file: hunk.file,
                    lineType, oldLine: curOldLine, newLine: curNewLine, lineContent: line,
                });
                newLineNum++;
            } else if (prefix === '-') {
                lines.push({
                    text: line, type: 'remove',
                    style: { bg: STYLE_REMOVE_BG, extendToLineEnd: true },
                    hunkId: hunk.id, file: hunk.file,
                    lineType, oldLine: curOldLine, newLine: curNewLine, lineContent: line,
                });
                oldLineNum++;
            } else {
                lines.push({
                    text: line, type: 'context',
                    hunkId: hunk.id, file: hunk.file,
                    lineType, oldLine: curOldLine, newLine: curNewLine, lineContent: line,
                });
                oldLineNum++;
                newLineNum++;
            }

            // Render inline comments attached to this line
            pushLineComments(lines, hunk, lineType, curOldLine, curNewLine);
        }
        }

        // Blank separator between files
        lines.push({ text: '', type: 'empty' });
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

// Theme colors for toolbar key hints
const STYLE_KEY_FG: OverlayColorSpec = "syntax.keyword";
const STYLE_KEY_BG: OverlayColorSpec = "editor.selection_bg";
const STYLE_HINT_FG: OverlayColorSpec = "editor.line_number_fg";
const STYLE_TOOLBAR_BG: OverlayColorSpec = "ui.status_bar_bg";
const STYLE_TOOLBAR_SEP: OverlayColorSpec = "ui.split_separator_fg";

interface HintItem {
    key: string;
    label: string;
}

/**
 * Build a styled toolbar entry with highlighted key hints.
 * Keys get bold + keyword color; labels get dim text; groups separated by │.
 */
function buildToolbarRow(W: number, groups: HintItem[][]): TextPropertyEntry {
    const overlays: InlineOverlay[] = [];
    let text = " ";
    let bytePos = getByteLength(" ");
    let done = false;

    for (let g = 0; g < groups.length && !done; g++) {
        if (g > 0) {
            const sep = " │ ";
            if (text.length + sep.length > W) { done = true; break; }
            overlays.push({ start: bytePos, end: bytePos + getByteLength(sep), style: { fg: STYLE_TOOLBAR_SEP } });
            text += sep;
            bytePos += getByteLength(sep);
        }
        for (let h = 0; h < groups[g].length && !done; h++) {
            const item = groups[g][h];
            const gap = h > 0 ? "  " : "";
            const fullLen = gap.length + item.key.length + 1 + item.label.length;
            const keyOnlyLen = gap.length + item.key.length;

            if (text.length + fullLen <= W) {
                if (gap) { text += gap; bytePos += getByteLength(gap); }
                const keyLen = getByteLength(item.key);
                overlays.push({ start: bytePos, end: bytePos + keyLen, style: { fg: STYLE_KEY_FG, bg: STYLE_KEY_BG, bold: true } });
                text += item.key;
                bytePos += keyLen;
                const labelText = " " + item.label;
                const labelLen = getByteLength(labelText);
                overlays.push({ start: bytePos, end: bytePos + labelLen, style: { fg: STYLE_HINT_FG } });
                text += labelText;
                bytePos += labelLen;
            } else if (text.length + keyOnlyLen <= W) {
                if (gap) { text += gap; bytePos += getByteLength(gap); }
                const keyLen = getByteLength(item.key);
                overlays.push({ start: bytePos, end: bytePos + keyLen, style: { fg: STYLE_KEY_FG, bg: STYLE_KEY_BG, bold: true } });
                text += item.key;
                bytePos += keyLen;
            } else {
                done = true;
            }
        }
    }

    const padded = text.padEnd(W) + "\n";
    return {
        text: padded,
        properties: { type: "toolbar" },
        style: { bg: STYLE_TOOLBAR_BG, extendToLineEnd: true },
        inlineOverlays: overlays,
    };
}

/**
 * Build the (two-row) toolbar with all review-diff shortcuts.
 * Row 1 — navigation; row 2 — actions. Identical regardless of which
 * panel currently has focus (no more files-pane vs diff-pane variants).
 */
function buildToolbar(W: number): TextPropertyEntry[] {
    const row1: HintItem[][] = [
        [{ key: "n", label: "next hunk" }, { key: "p", label: "prev hunk" }],
        [{ key: "s", label: "stage" }, { key: "u", label: "unstage" }, { key: "d", label: "discard" }, { key: "c", label: "comment" }],
    ];
    const row2: HintItem[][] = [
        [{ key: "Tab", label: "fold file" }, { key: "z a", label: "fold all" }, { key: "z r", label: "unfold all" }],
        [{ key: "Enter", label: "jump to source" }, { key: "e", label: "export" }, { key: "q", label: "close" }],
    ];
    return [buildToolbarRow(W, row1), buildToolbarRow(W, row2)];
}

// --- Buffer Group panel content builders ---

function buildToolbarPanelEntries(): TextPropertyEntry[] {
    // Two-row toolbar: navigation hints on row 1, actions on row 2.
    return buildToolbar(state.viewportWidth);
}

/**
 * Build the unified-diff stream entries. Emits one row per file header
 * followed by all of that file's hunks inline, plus inline comments and
 * a blank separator between files. As a side effect, populates
 * `state.hunkHeaderRows`, `state.diffLineByteOffsets`, and
 * `state.fileHeaderRows` so the rest of the plugin can map cursor rows
 * back to hunks/files.
 */
function buildDiffPanelEntries(): TextPropertyEntry[] {
    const entries: TextPropertyEntry[] = [];

    const hunkHeaderRows: number[] = [];
    const diffLineByteOffsets: number[] = [];
    const fileHeaderRows: Record<string, number> = {};
    let runningByte = 0;
    let row = 0; // 0-indexed counter; row + 1 is the 1-indexed line number

    const pushEntry = (entry: TextPropertyEntry) => {
        diffLineByteOffsets.push(runningByte);
        runningByte += getByteLength(entry.text);
        entries.push(entry);
        row++;
    };

    const lines = buildDiffLines(state.viewportWidth);
    for (const line of lines) {
        const props: Record<string, unknown> = { type: line.type };
        if (line.hunkId !== undefined) props.hunkId = line.hunkId;
        if (line.file !== undefined) props.file = line.file;
        if (line.lineType !== undefined) props.lineType = line.lineType;
        if (line.oldLine !== undefined) props.oldLine = line.oldLine;
        if (line.newLine !== undefined) props.newLine = line.newLine;
        if (line.lineContent !== undefined) props.lineContent = line.lineContent;
        if (line.commentId !== undefined) props.commentId = line.commentId;
        if (line.filePath !== undefined) props.filePath = line.filePath;
        if (line.fileKey !== undefined) props.fileKey = line.fileKey;
        if (line.fileIndex !== undefined) props.fileIndex = line.fileIndex;

        if (line.type === 'hunk-header') {
            hunkHeaderRows.push(row + 1);
        }
        if (line.type === 'file-header' && line.fileKey) {
            fileHeaderRows[line.fileKey] = row + 1;
        }

        pushEntry({
            text: (line.text || "") + "\n",
            style: line.style,
            inlineOverlays: line.inlineOverlays,
            properties: props,
        });
    }

    diffLineByteOffsets.push(runningByte);

    state.hunkHeaderRows = hunkHeaderRows;
    state.diffLineByteOffsets = diffLineByteOffsets;
    state.fileHeaderRows = fileHeaderRows;
    return entries;
}

/**
 * Build the comments navigation panel. Flat list of comments in the
 * order they appear in the unified diff stream. Each row reads
 *   "path:line  snippet"
 * truncated to fit the panel width. Empty state shows a dim "No comments
 * yet." line. Read-only in this step (interaction lands in Step 5/6).
 */
function buildCommentsPanelEntries(): TextPropertyEntry[] {
    const entries: TextPropertyEntry[] = [];
    state.commentsByRow = {};

    const headerLabel = editor.t("panel.comments") || "Comments";
    entries.push({
        text: ` ${headerLabel}\n`,
        style: { fg: STYLE_HEADER, bold: true, underline: true },
        properties: { type: "header" },
    });

    if (state.comments.length === 0) {
        entries.push({
            text: ` ${editor.t("panel.no_comments") || "No comments yet."}\n`,
            style: { fg: STYLE_SECTION_HEADER, italic: true },
            properties: { type: "empty" },
        });
        return entries;
    }

    // Order comments by their position in the unified stream. We approximate
    // by sorting by (file index, line number, removed/added preference).
    const fileIndex = (file: string, category: string | undefined): number => {
        for (let i = 0; i < state.files.length; i++) {
            const f = state.files[i];
            if (f.path === file) return i;
        }
        return Number.MAX_SAFE_INTEGER;
    };

    const sortedComments = [...state.comments].sort((a, b) => {
        // Look up via hunk's file
        const hunkA = state.hunks.find(h => h.id === a.hunk_id);
        const hunkB = state.hunks.find(h => h.id === b.hunk_id);
        const fa = fileIndex(a.file, hunkA?.gitStatus);
        const fb = fileIndex(b.file, hunkB?.gitStatus);
        if (fa !== fb) return fa - fb;
        const la = a.new_line ?? a.old_line ?? 0;
        const lb = b.new_line ?? b.old_line ?? 0;
        return la - lb;
    });

    let rowIdx = 1; // header is row 0 (0-indexed); comments start at row 1
    for (const c of sortedComments) {
        rowIdx++;
        const lineRef = c.new_line ?? c.old_line ?? 0;
        const path = c.file.split('/').pop() || c.file;
        const snippet = c.text.replace(/\s+/g, ' ').trim();
        const text = ` ${path}:${lineRef}  ${snippet}`;

        // Truncate to fit panel width (estimate).
        const panelWidth = Math.max(20, Math.floor(state.viewportWidth * 0.25) - 2);
        const display = text.length > panelWidth ? text.slice(0, panelWidth - 1) + '…' : text;

        const isSelected = rowIdx === state.commentsSelectedRow && state.focusPanel === 'comments';
        const style: Partial<OverlayOptions> | undefined = isSelected
            ? { bg: STYLE_SELECTED_BG, bold: true, extendToLineEnd: true }
            : undefined;

        // Color the path:line prefix in keyword color
        const prefixLen = getByteLength(` ${path}:${lineRef}`);
        const inlineOverlays: InlineOverlay[] = [
            { start: 1, end: prefixLen, style: { fg: STYLE_KEY_FG } },
        ];

        state.commentsByRow[rowIdx] = c.id;
        entries.push({
            text: display + "\n",
            style,
            inlineOverlays,
            properties: { type: "comment-nav", commentId: c.id, file: c.file, line: lineRef },
        });
    }

    return entries;
}

/**
 * Full refresh — rebuild all three panels. Called on data changes
 * (refreshMagitData, comment add/edit, note edit, resize). NOT called on
 * scroll: scrolling is handled natively by the editor in the panel buffers.
 */
function updateMagitDisplay(): void {
    refreshViewportDimensions();
    if (state.groupId === null) return;
    editor.setPanelContent(state.groupId, "toolbar", buildToolbarPanelEntries());
    editor.setPanelContent(state.groupId, "diff", buildDiffPanelEntries());
    editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
    refreshStickyHeader(0);
    applyCursorLineOverlay('diff');
}

/**
 * Render the sticky panel for `topVisibleRow` (0-indexed line at the top
 * of the diff viewport). Shows the file whose header row is the largest
 * ≤ topVisibleRow, with its category as a dim prefix. Falls back to a
 * neutral summary when nothing is above the cursor.
 */
function refreshStickyHeader(topVisibleRow: number): void {
    if (state.groupId === null) return;
    const stickyId = state.panelBuffers["sticky"];
    if (stickyId === undefined) return;

    const W = state.viewportWidth;
    let text: string;
    let style: Partial<OverlayOptions> = { fg: STYLE_HEADER, bold: true };

    // topVisibleRow is 0-indexed; fileHeaderRows are 1-indexed.
    const top1 = topVisibleRow + 1;
    let bestFile: FileEntry | null = null;
    let bestRow = 0;
    for (const f of state.files) {
        const row = state.fileHeaderRows[fileKey(f)];
        if (row !== undefined && row <= top1 && row > bestRow) {
            bestRow = row;
            bestFile = f;
        }
    }

    if (!bestFile) {
        if (state.files.length === 0) {
            text = ` ${editor.t("status.review_empty") || "Review Diff"}`;
        } else {
            const totals = state.files.reduce(
                (acc, f) => {
                    const c = fileChangeCounts(f);
                    acc.added += c.added;
                    acc.removed += c.removed;
                    return acc;
                },
                { added: 0, removed: 0 }
            );
            text = ` Review Diff — ${state.files.length} files, +${totals.added} / -${totals.removed}`;
            style = { fg: STYLE_SECTION_HEADER, italic: true };
        }
    } else {
        const counts = fileChangeCounts(bestFile);
        let section: string = bestFile.category;
        if (bestFile.category === 'staged') section = (editor.t("section.staged") || "Staged").toUpperCase();
        else if (bestFile.category === 'unstaged') section = (editor.t("section.unstaged") || "Changes").toUpperCase();
        else if (bestFile.category === 'untracked') section = (editor.t("section.untracked") || "Untracked").toUpperCase();
        const filename = bestFile.origPath ? `${bestFile.origPath} → ${bestFile.path}` : bestFile.path;
        text = ` ${section} · ${filename}   +${counts.added} / -${counts.removed}`;
    }

    const padded = (text.length > W ? text.slice(0, W) : text).padEnd(W) + "\n";
    editor.setPanelContent(state.groupId, "sticky", [{
        text: padded,
        style: { ...style, bg: STYLE_TOOLBAR_BG, extendToLineEnd: true },
        properties: { type: "sticky-header" },
    }]);
}

/**
 * Helper: jump the diff cursor to the file's first hunk (or its file
 * header if it has no hunks). Auto-expands the file if collapsed.
 */
function jumpToFile(file: FileEntry): void {
    const key = fileKey(file);
    if (state.collapsedFiles.has(key)) {
        state.collapsedFiles.delete(key);
        updateMagitDisplay();
    }
    // Prefer first hunk row; fall back to the file-header row.
    const fileIdx = state.files.indexOf(file);
    if (fileIdx >= 0) {
        // Compute visible hunk index of the first hunk for this file.
        let visibleIdx = 0;
        let foundGlobal = -1;
        for (let i = 0; i < state.hunks.length; i++) {
            const h = state.hunks[i];
            const hKey = fileKeyOf(h.file, h.gitStatus || 'unstaged');
            if (state.collapsedFiles.has(hKey)) continue;
            if (h.file === file.path && h.gitStatus === file.category) {
                foundGlobal = i;
                break;
            }
            visibleIdx++;
        }
        if (foundGlobal >= 0) {
            const row = state.hunkHeaderRows[visibleIdx];
            if (row !== undefined) { jumpDiffCursorToRow(row); return; }
        }
    }
    const headerRow = state.fileHeaderRows[key];
    if (headerRow !== undefined) jumpDiffCursorToRow(headerRow);
}

/**
 * Mouse click handler. Routes clicks to the appropriate behavior:
 *   * Diff buffer file-header row → toggle that file's collapse state.
 *   * Sticky panel → jump to the currently-pinned file's first hunk.
 *   * Comments panel row → jump diff cursor to that comment's location
 *     (auto-expanding the file when collapsed) and select the row.
 */
function on_review_mouse_click(data: {
    column: number; row: number; button: string; modifiers: string;
    content_x: number; content_y: number;
    buffer_id: number | null; buffer_row: number | null; buffer_col: number | null;
}): void {
    if (state.groupId === null) return;
    if (data.buffer_id === null || data.buffer_row === null) return;

    const diffId = state.panelBuffers["diff"];
    const stickyId = state.panelBuffers["sticky"];
    const commentsId = state.panelBuffers["comments"];

    // Click in the diff buffer: only the file-header row is interactive.
    if (data.buffer_id === diffId) {
        const targetRow1 = data.buffer_row + 1;
        for (const f of state.files) {
            if (state.fileHeaderRows[fileKey(f)] === targetRow1) {
                const key = fileKey(f);
                if (state.collapsedFiles.has(key)) state.collapsedFiles.delete(key);
                else state.collapsedFiles.add(key);
                updateMagitDisplay();
                return;
            }
        }
        return;
    }

    // Click on the sticky pinned-header: jump to the pinned file's first hunk.
    if (data.buffer_id === stickyId) {
        // Re-derive the pinned file from current viewport top.
        const top1 = state.diffCursorRow; // approximation; sticky tracks topmost visible
        let bestFile: FileEntry | null = null;
        let bestRow = 0;
        for (const f of state.files) {
            const row = state.fileHeaderRows[fileKey(f)];
            if (row !== undefined && row <= top1 && row > bestRow) {
                bestRow = row;
                bestFile = f;
            }
        }
        if (bestFile) jumpToFile(bestFile);
        return;
    }

    // Click in the comments panel: jump to the comment's location.
    if (data.buffer_id === commentsId) {
        const targetRow1 = data.buffer_row + 1;
        const commentId = state.commentsByRow[targetRow1];
        if (commentId) {
            jumpToComment(commentId);
            state.commentsSelectedRow = targetRow1;
            editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
        }
        return;
    }
}
registerHandler("on_review_mouse_click", on_review_mouse_click);

/**
 * Jump the diff cursor to the line associated with a comment, auto-
 * expanding the comment's file if it is currently collapsed.
 */
function jumpToComment(commentId: string): void {
    const comment = state.comments.find(c => c.id === commentId);
    if (!comment) return;
    const hunk = state.hunks.find(h => h.id === comment.hunk_id);
    if (!hunk) return;
    const file = state.files.find(f => f.path === hunk.file && f.category === hunk.gitStatus);
    if (file) {
        const key = fileKey(file);
        if (state.collapsedFiles.has(key)) {
            state.collapsedFiles.delete(key);
            updateMagitDisplay();
        }
    }
    // Find the row of the hunk in the rebuilt buffer.
    let visibleIdx = 0;
    let foundGlobal = -1;
    for (let i = 0; i < state.hunks.length; i++) {
        const h = state.hunks[i];
        if (state.collapsedFiles.has(fileKeyOf(h.file, h.gitStatus || 'unstaged'))) continue;
        if (h.id === hunk.id) { foundGlobal = visibleIdx; break; }
        visibleIdx++;
    }
    if (foundGlobal < 0) return;
    const hunkRow = state.hunkHeaderRows[foundGlobal];
    if (hunkRow === undefined) return;
    // Best-effort: jump to the hunk header. Lining up to the exact line
    // would require re-parsing the diff line offsets — the hunk header is
    // close enough that the user can find their line in one glance.
    jumpDiffCursorToRow(hunkRow);
}

function on_review_viewport_changed(data: { split_id: number; buffer_id: number; top_byte: number; top_line: number | null; width: number; height: number }): void {
    if (state.groupId === null) return;
    if (data.buffer_id !== state.panelBuffers["diff"]) return;
    const topRow = data.top_line ?? 0;
    refreshStickyHeader(topRow);
}
registerHandler("on_review_viewport_changed", on_review_viewport_changed);

/**
 * Repaint the synthetic "cursor line" highlight in the diff panel.
 *
 * The diff panel buffer is created with show_cursors=true so the editor
 * moves the cursor natively, but a single-line bg overlay on the cursor row
 * gives a much more visible "you are here" indicator than the bare caret —
 * which matches the magit-style aesthetic and is what the user expects.
 */
function applyCursorLineOverlay(panel: 'diff'): void {
    const bufId = state.panelBuffers[panel];
    if (bufId === undefined) return;
    editor.clearNamespace(bufId, CURSOR_LINE_NS);
    const offsets = state.diffLineByteOffsets;
    if (offsets.length < 2) return;
    const idx = Math.max(0, Math.min(state.diffCursorRow - 1, offsets.length - 2));
    const start = offsets[idx];
    const end = offsets[idx + 1];
    if (end <= start) return;
    editor.addOverlay(bufId, CURSOR_LINE_NS, start, end, {
        bg: STYLE_SELECTED_BG,
        extendToLineEnd: true,
    });
}

function review_refresh() { refreshMagitData(); }
registerHandler("review_refresh", review_refresh);

// --- Cursor-driven navigation ---
//
// In the unified-stream layout the diff panel owns the editor's native
// cursor; j/k/Up/Down/PageUp/PageDown/Home/End delegate directly to the
// editor's built-in motion actions via `executeAction`. The plugin only
// observes `cursor_moved` events to repaint the cursor-line overlay and
// keep `state.diffCursorRow` in sync.

/**
 * Derive the "current file" (FileEntry) from the cursor row in the unified
 * diff stream — the file whose header row is the largest one ≤ the cursor
 * row. Returns null if no file header is at or above the cursor (cursor
 * sits in the empty preamble or there are no files).
 */
function currentFileFromCursor(): FileEntry | null {
    let bestFile: FileEntry | null = null;
    let bestRow = 0;
    for (const f of state.files) {
        const row = state.fileHeaderRows[fileKey(f)];
        if (row !== undefined && row <= state.diffCursorRow && row > bestRow) {
            bestRow = row;
            bestFile = f;
        }
    }
    return bestFile;
}

function review_toggle_file_collapse() {
    if (state.groupId === null) return;
    // Tab from the comments panel swaps focus back to the diff stream.
    if (state.focusPanel === 'comments') {
        editor.focusBufferGroupPanel(state.groupId, "diff");
        return;
    }
    if (state.files.length === 0) return;
    // Determine which file the cursor is on. Prefer a file-header row
    // directly under the cursor; fall back to the file the cursor sits
    // in (so Tab on a hunk row also collapses the parent file).
    const headerFile = fileHeaderUnderCursor() ?? currentFileFromCursor();
    if (!headerFile) return;
    const key = fileKey(headerFile);
    if (state.collapsedFiles.has(key)) state.collapsedFiles.delete(key);
    else state.collapsedFiles.add(key);
    updateMagitDisplay();
    // Move cursor to the file header row so the user sees what they
    // collapsed/expanded.
    const headerRow = state.fileHeaderRows[key];
    if (headerRow !== undefined) jumpDiffCursorToRow(headerRow);
}
registerHandler("review_toggle_file_collapse", review_toggle_file_collapse);

/**
 * Order comments the same way the comments panel does — by file order
 * in the unified stream, then by line number. Keeping the ordering
 * here in sync with `buildCommentsPanelEntries` is important so that
 * keyboard navigation lands on the same row the user sees.
 */
function commentsInPanelOrder(): ReviewComment[] {
    const fileIndex = (file: string): number => {
        for (let i = 0; i < state.files.length; i++) {
            if (state.files[i].path === file) return i;
        }
        return Number.MAX_SAFE_INTEGER;
    };
    return [...state.comments].sort((a, b) => {
        const fa = fileIndex(a.file);
        const fb = fileIndex(b.file);
        if (fa !== fb) return fa - fb;
        return (a.new_line ?? a.old_line ?? 0) - (b.new_line ?? b.old_line ?? 0);
    });
}

function selectAndJumpToComment(c: ReviewComment) {
    if (state.groupId === null) return;
    jumpToComment(c.id);
    // Find the comment's row in the panel (header is row 1, comments start at 2).
    const sorted = commentsInPanelOrder();
    const idx = sorted.findIndex(x => x.id === c.id);
    if (idx >= 0) {
        state.commentsSelectedRow = idx + 2;
        editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
    }
}

function review_next_comment() {
    if (state.comments.length === 0) {
        editor.setStatus(editor.t("status.no_comments") || "No comments");
        return;
    }
    const sorted = commentsInPanelOrder();
    // Determine the comment-id currently under the diff cursor (if any).
    const currentRow = state.commentsSelectedRow;
    const currentIdx = currentRow >= 2 ? currentRow - 2 : -1;
    const nextIdx = Math.min(sorted.length - 1, currentIdx + 1);
    if (nextIdx === currentIdx && currentIdx >= 0) return;
    selectAndJumpToComment(sorted[nextIdx >= 0 ? nextIdx : 0]);
}
registerHandler("review_next_comment", review_next_comment);

function review_prev_comment() {
    if (state.comments.length === 0) {
        editor.setStatus(editor.t("status.no_comments") || "No comments");
        return;
    }
    const sorted = commentsInPanelOrder();
    const currentRow = state.commentsSelectedRow;
    const currentIdx = currentRow >= 2 ? currentRow - 2 : sorted.length;
    const prevIdx = Math.max(0, currentIdx - 1);
    selectAndJumpToComment(sorted[prevIdx]);
}
registerHandler("review_prev_comment", review_prev_comment);

/**
 * Focus the comments panel. Uses native focus-swap so the buffer's
 * native cursor takes the keystrokes (j/k/Enter handled by the
 * comments-mode keybindings).
 */
function review_focus_comments() {
    if (state.groupId === null) return;
    editor.focusBufferGroupPanel(state.groupId, "comments");
    // Ensure the selection highlight shows immediately.
    if (state.commentsSelectedRow < 2 && state.comments.length > 0) {
        state.commentsSelectedRow = 2;
    }
    editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
}
registerHandler("review_focus_comments", review_focus_comments);

/**
 * Activate the currently-selected comment in the comments panel:
 * jump the diff cursor to it (auto-expanding the file if collapsed).
 */
function review_open_selected_comment() {
    if (state.commentsSelectedRow < 2) return;
    const commentId = state.commentsByRow[state.commentsSelectedRow];
    if (!commentId) return;
    jumpToComment(commentId);
}
registerHandler("review_open_selected_comment", review_open_selected_comment);

function review_comments_select_next() {
    if (state.groupId === null) return;
    if (state.comments.length === 0) return;
    const total = state.comments.length;
    const currentIdx = Math.max(0, state.commentsSelectedRow - 2);
    const nextIdx = Math.min(total - 1, currentIdx + 1);
    state.commentsSelectedRow = nextIdx + 2;
    editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
}
registerHandler("review_comments_select_next", review_comments_select_next);

function review_enter_dispatch() {
    if (state.focusPanel === 'comments') {
        review_open_selected_comment();
        return;
    }
    review_drill_down();
}
registerHandler("review_enter_dispatch", review_enter_dispatch);

function review_comments_select_prev() {
    if (state.groupId === null) return;
    if (state.comments.length === 0) return;
    const currentIdx = Math.max(0, state.commentsSelectedRow - 2);
    const prevIdx = Math.max(0, currentIdx - 1);
    state.commentsSelectedRow = prevIdx + 2;
    editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
}
registerHandler("review_comments_select_prev", review_comments_select_prev);

/**
 * Visual line-selection mode. Activates a multi-row selection rooted
 * at the cursor's hunk; j/k extend it; Esc cancels. The selection is
 * rendered as an inverted background overlay across the selected rows.
 */
function review_visual_start() {
    if (state.groupId === null) return;
    const props = readPropsAtCursor('diff');
    if (!props) return;
    const hunkId = props["hunkId"];
    const lineType = props["lineType"];
    if (typeof hunkId !== 'string' || (lineType !== 'add' && lineType !== 'remove' && lineType !== 'context')) {
        editor.setStatus(editor.t("status.visual_no_diff_line") || "Visual selection requires a diff line");
        return;
    }
    state.lineSelection = {
        startRow: state.diffCursorRow,
        endRow: state.diffCursorRow,
        hunkId,
    };
    paintLineSelectionOverlay();
    editor.setStatus(editor.t("status.visual_started") || "Visual: j/k extend, s/u/d apply, Esc cancel");
}
registerHandler("review_visual_start", review_visual_start);

function review_visual_cancel() {
    state.lineSelection = null;
    if (state.groupId !== null) {
        const diffId = state.panelBuffers["diff"];
        if (diffId !== undefined) editor.clearNamespace(diffId, "review-line-selection");
    }
    applyCursorLineOverlay('diff');
}
registerHandler("review_visual_cancel", review_visual_cancel);

const LINE_SELECTION_NS = "review-line-selection";

function paintLineSelectionOverlay() {
    if (state.groupId === null) return;
    const diffId = state.panelBuffers["diff"];
    if (diffId === undefined) return;
    editor.clearNamespace(diffId, LINE_SELECTION_NS);
    if (!state.lineSelection) return;
    const { startRow, endRow } = state.lineSelection;
    const lo = Math.min(startRow, endRow);
    const hi = Math.max(startRow, endRow);
    for (let r = lo; r <= hi; r++) {
        const idx = r - 1;
        if (idx < 0 || idx + 1 >= state.diffLineByteOffsets.length) continue;
        const start = state.diffLineByteOffsets[idx];
        const end = state.diffLineByteOffsets[idx + 1];
        if (end <= start) continue;
        editor.addOverlay(diffId, LINE_SELECTION_NS, start, end, {
            bg: STYLE_SELECTED_BG,
            extendToLineEnd: true,
        });
    }
}

/**
 * Translate the active line-selection's (startRow, endRow) into a
 * lineRange (inclusive 0-indexed indices into `hunk.lines`) by walking
 * the rows of the unified stream that belong to the selection's hunk.
 *
 * Returns `null` if the selection crosses out of its hunk (which can't
 * happen given how j/k extend, but defensively guarded), or the hunk
 * can't be found, or the selection contains only context lines (which
 * makes stage/unstage a no-op).
 */
function selectionLineRange(): { hunk: Hunk; range: { start: number; end: number } } | null {
    if (!state.lineSelection) return null;
    const sel = state.lineSelection;
    const hunk = state.hunks.find(h => h.id === sel.hunkId);
    if (!hunk) return null;
    // Find the row of this hunk's header in the unified stream.
    const hunkIdx = state.hunks.indexOf(hunk);
    let visibleIdx = 0;
    for (let i = 0; i < hunkIdx; i++) {
        const h = state.hunks[i];
        if (state.collapsedFiles.has(fileKeyOf(h.file, h.gitStatus || 'unstaged'))) continue;
        visibleIdx++;
    }
    const headerRow = state.hunkHeaderRows[visibleIdx];
    if (headerRow === undefined) return null;

    const lo = Math.min(sel.startRow, sel.endRow);
    const hi = Math.max(sel.startRow, sel.endRow);
    const startInHunk = lo - headerRow - 1; // -1 because the header row itself is not in hunk.lines
    const endInHunk = hi - headerRow - 1;
    if (startInHunk < 0 || endInHunk >= hunk.lines.length) return null;

    // Reject context-only selections.
    let hasChange = false;
    for (let i = startInHunk; i <= endInHunk; i++) {
        const ch = hunk.lines[i][0];
        if (ch === '+' || ch === '-') { hasChange = true; break; }
    }
    if (!hasChange) return null;

    return { hunk, range: { start: startInHunk, end: endInHunk } };
}

async function applyLineSelection(action: 'stage' | 'unstage' | 'discard') {
    const sel = selectionLineRange();
    if (!sel) {
        editor.setStatus(editor.t("status.visual_invalid") || "Selection has no add/remove lines or crosses hunk boundary");
        return;
    }
    const { hunk, range } = sel;
    const patch = buildHunkPatch(hunk.file, hunk, range);
    let flags: string[];
    if (action === 'stage') flags = ["--cached", "--unidiff-zero"];
    else if (action === 'unstage') flags = ["--cached", "--reverse", "--unidiff-zero"];
    else flags = ["--reverse", "--unidiff-zero"];

    rememberPendingHunkAnchor(hunk.id);
    const ok = await applyHunkPatch(patch, flags);
    if (!ok) return;
    review_visual_cancel();
    editor.setStatus(editor.t(`status.lines_${action}d`) || `Lines ${action}d`);
    await refreshMagitData();
}

function review_collapse_all() {
    state.collapsedFiles = new Set(state.files.map(fileKey));
    updateMagitDisplay();
}
registerHandler("review_collapse_all", review_collapse_all);

function review_expand_all() {
    state.collapsedFiles.clear();
    updateMagitDisplay();
}
registerHandler("review_expand_all", review_expand_all);

function review_nav_up() {
    if (state.focusPanel === 'comments') { review_comments_select_prev(); return; }
    editor.executeAction("move_up");
    if (state.lineSelection) {
        // executeAction has already moved the cursor; sync the selection.
        // Ensure we don't extend out of the hunk.
        const newRow = Math.max(1, state.lineSelection.endRow - 1);
        state.lineSelection.endRow = newRow;
        paintLineSelectionOverlay();
    }
}
registerHandler("review_nav_up", review_nav_up);

function review_nav_down() {
    if (state.focusPanel === 'comments') { review_comments_select_next(); return; }
    editor.executeAction("move_down");
    if (state.lineSelection) {
        state.lineSelection.endRow = state.lineSelection.endRow + 1;
        paintLineSelectionOverlay();
    }
}
registerHandler("review_nav_down", review_nav_down);

function review_page_up() { editor.executeAction("move_page_up"); }
registerHandler("review_page_up", review_page_up);

function review_page_down() { editor.executeAction("move_page_down"); }
registerHandler("review_page_down", review_page_down);

function review_nav_home() { editor.executeAction("move_document_start"); }
registerHandler("review_nav_home", review_nav_home);

function review_nav_end() { editor.executeAction("move_document_end"); }
registerHandler("review_nav_end", review_nav_end);

// --- Real git stage/unstage/discard actions (Step 4) ---

/**
 * Build a minimal unified diff patch for a single hunk.
 *
 * When `lineRange` is provided, only the +/- lines whose indices fall
 * inside the inclusive range are kept; +/- lines outside the range are
 * converted to context lines so that the patch still applies cleanly
 * to the file. Context lines are always preserved.
 */
function buildHunkPatch(filePath: string, hunk: Hunk, lineRange?: { start: number; end: number }): string {
    const filtered: string[] = [];
    let oldCount = 0;
    let newCount = 0;

    for (let i = 0; i < hunk.lines.length; i++) {
        const line = hunk.lines[i];
        const ch = line[0];
        const inRange = !lineRange || (i >= lineRange.start && i <= lineRange.end);
        if (ch === '+') {
            if (inRange) {
                filtered.push(line);
                newCount++;
            } else {
                // An out-of-range '+' line means: this addition isn't being
                // applied, so it shouldn't appear in either side. Drop it
                // entirely (don't convert to context — there's nothing to
                // match in the source file).
            }
        } else if (ch === '-') {
            if (inRange) {
                filtered.push(line);
                oldCount++;
            } else {
                // An out-of-range '-' line: this deletion isn't applied,
                // so the line still exists on both sides — render as context.
                filtered.push(' ' + line.substring(1));
                oldCount++;
                newCount++;
            }
        } else {
            filtered.push(line);
            oldCount++;
            newCount++;
        }
    }

    const header = `@@ -${hunk.oldRange.start},${oldCount} +${hunk.range.start},${newCount} @@`;
    return [
        `diff --git a/${filePath} b/${filePath}`,
        `--- a/${filePath}`,
        `+++ b/${filePath}`,
        header,
        ...filtered,
        ''
    ].join('\n');
}

/**
 * Write a patch to a temp file and apply it with the given flags.
 * Returns true on success.
 */
async function applyHunkPatch(patch: string, flags: string[]): Promise<boolean> {
    const tmpDir = editor.getTempDir();
    const patchPath = editor.pathJoin(tmpDir, `fresh-review-${Date.now()}.patch`);
    editor.writeFile(patchPath, patch);
    // Validate first
    const check = await editor.spawnProcess("git", ["apply", "--check", ...flags, patchPath]);
    if (check.exit_code !== 0) {
        editor.setStatus("Patch failed: " + (check.stderr || "").trim());
        return false;
    }
    const result = await editor.spawnProcess("git", ["apply", ...flags, patchPath]);
    return result.exit_code === 0;
}

/**
 * Merge all text-property records at the cursor of the given panel buffer
 * into a single object. There's typically only one record covering each
 * cursor position; merging keeps callers simple.
 */
function readPropsAtCursor(panel: 'files' | 'diff'): Record<string, unknown> | null {
    const bufId = state.panelBuffers[panel];
    if (bufId === undefined) return null;
    const records = editor.getTextPropertiesAtCursor(bufId);
    if (!records || records.length === 0) return null;
    const merged: Record<string, unknown> = {};
    for (const r of records) Object.assign(merged, r);
    return merged;
}

/**
 * Get the hunk under the cursor in the diff panel, or null.
 *
 * Reads the `hunkId` text property embedded by `buildDiffPanelEntries`. Falls
 * back to the first hunk of the selected file when the cursor is somewhere
 * without a hunkId (e.g. the panel header) so commands like `s` still do
 * something useful.
 */
function getHunkAtDiffCursor(): Hunk | null {
    const props = readPropsAtCursor('diff');
    const hunkId = props ? props["hunkId"] : undefined;
    if (typeof hunkId === 'string') {
        const found = state.hunks.find(h => h.id === hunkId);
        if (found) return found;
    }
    // Fallback: first hunk for the file under the cursor (if any).
    const cur = currentFileFromCursor();
    if (!cur) return null;
    return state.hunks.find(
        h => h.file === cur.path && h.gitStatus === cur.category
    ) || null;
}

/**
 * Determine if the cursor is on a file-header row. Returns the FileEntry
 * if so, otherwise null.
 */
function fileHeaderUnderCursor(): FileEntry | null {
    const props = readPropsAtCursor('diff');
    if (!props || props["type"] !== 'file-header') return null;
    const filePath = props["filePath"];
    if (typeof filePath !== 'string') return null;
    return state.files.find(f => f.path === filePath) || null;
}

/**
 * Stage at the appropriate scope based on cursor context:
 *   * file header  → stage the whole file
 *   * hunk         → stage just that hunk
 */
async function review_stage_scope() {
    if (state.files.length === 0) return;
    if (state.lineSelection) { await applyLineSelection('stage'); return; }
    const headerFile = fileHeaderUnderCursor();
    if (headerFile) {
        await stageFileEntry(headerFile);
        return;
    }
    await stageHunk(getHunkAtDiffCursor());
}
registerHandler("review_stage_scope", review_stage_scope);

async function review_unstage_scope() {
    if (state.files.length === 0) return;
    if (state.lineSelection) { await applyLineSelection('unstage'); return; }
    const headerFile = fileHeaderUnderCursor();
    if (headerFile) {
        await unstageFileEntry(headerFile);
        return;
    }
    await unstageHunk(getHunkAtDiffCursor());
}
registerHandler("review_unstage_scope", review_unstage_scope);

/**
 * Always-file-level staging (S / U). Acts on the file the cursor is
 * currently inside, regardless of whether it's on a header or a hunk.
 */
async function review_stage_file() {
    if (state.files.length === 0) return;
    const f = fileHeaderUnderCursor() ?? currentFileFromCursor();
    if (!f) return;
    await stageFileEntry(f);
}
registerHandler("review_stage_file", review_stage_file);

async function review_unstage_file() {
    if (state.files.length === 0) return;
    const f = fileHeaderUnderCursor() ?? currentFileFromCursor();
    if (!f) return;
    await unstageFileEntry(f);
}
registerHandler("review_unstage_file", review_unstage_file);

async function stageFileEntry(f: FileEntry) {
    rememberPendingHunkAnchor(null);
    await editor.spawnProcess("git", ["add", "--", f.path]);
    await refreshMagitData();
}

async function unstageFileEntry(f: FileEntry) {
    rememberPendingHunkAnchor(null);
    await editor.spawnProcess("git", ["reset", "HEAD", "--", f.path]);
    await refreshMagitData();
}

async function stageHunk(hunk: Hunk | null) {
    if (!hunk || !hunk.file) return;
    rememberPendingHunkAnchor(hunk.id);
    if (hunk.gitStatus === 'untracked') {
        await editor.spawnProcess("git", ["add", "--", hunk.file]);
    } else {
        const patch = buildHunkPatch(hunk.file, hunk);
        const ok = await applyHunkPatch(patch, ["--cached"]);
        if (!ok) return;
    }
    editor.setStatus(editor.t("status.hunk_staged") || "Hunk staged");
    await refreshMagitData();
}

async function unstageHunk(hunk: Hunk | null) {
    if (!hunk || !hunk.file || hunk.gitStatus !== 'staged') {
        editor.setStatus("Can only unstage staged hunks");
        return;
    }
    rememberPendingHunkAnchor(hunk.id);
    const patch = buildHunkPatch(hunk.file, hunk);
    const ok = await applyHunkPatch(patch, ["--cached", "--reverse"]);
    if (!ok) return;
    editor.setStatus(editor.t("status.hunk_unstaged") || "Hunk unstaged");
    await refreshMagitData();
}

/**
 * Cursor continuity: remember the hunk-id we just acted on so that
 * after the rebuild we can land the cursor back on the same hunk
 * (which may have moved between sections), or on the nearest survivor.
 */
let pendingHunkAnchor: { hunkId: string | null; section: string | null; row: number } | null = null;
function rememberPendingHunkAnchor(hunkId: string | null) {
    const cur = getHunkAtDiffCursor();
    pendingHunkAnchor = {
        hunkId,
        section: cur?.gitStatus ?? null,
        row: state.diffCursorRow,
    };
}

let pendingDiscardFile: FileEntry | null = null;

/** Always-file-level discard (D). Acts on the file the cursor is in. */
function review_discard_file_only() {
    if (state.files.length === 0) return;
    const f = fileHeaderUnderCursor() ?? currentFileFromCursor();
    if (!f) return;
    pendingDiscardFile = f;
    rememberPendingHunkAnchor(null);
    const action = f.category === 'untracked' ? "Delete" : "Discard changes in";
    editor.startPrompt(`${action} "${f.path}"? This cannot be undone.`, "review-discard-confirm");
    const suggestions: PromptSuggestion[] = [
        { text: `${action} file`, description: "Permanently lose changes", value: "discard" },
        { text: "Cancel", description: "Keep the file as-is", value: "cancel" },
    ];
    editor.setPromptSuggestions(suggestions);
}
registerHandler("review_discard_file_only", review_discard_file_only);

function review_discard_file() {
    if (state.files.length === 0) return;
    if (state.lineSelection) { void applyLineSelection('discard'); return; }
    const headerFile = fileHeaderUnderCursor();
    const f = headerFile ?? currentFileFromCursor();
    if (!headerFile) {
        // No file-header under cursor → hunk-level discard
        const hunk = getHunkAtDiffCursor();
        if (!hunk || !hunk.file) return;
        rememberPendingHunkAnchor(hunk.id);
        editor.startPrompt(
            editor.t("prompt.discard_hunk", { file: hunk.file }) ||
            `Discard this hunk in "${hunk.file}"? This cannot be undone.`,
            "review-discard-hunk-confirm"
        );
        const suggestions: PromptSuggestion[] = [
            { text: "Discard hunk", description: "Permanently lose this change", value: "discard" },
            { text: "Cancel", description: "Keep the hunk as-is", value: "cancel" },
        ];
        editor.setPromptSuggestions(suggestions);
        return;
    }
    if (!f) return;

    // Show confirmation prompt — discard is destructive and irreversible
    pendingDiscardFile = f;
    rememberPendingHunkAnchor(null);
    const action = f.category === 'untracked' ? "Delete" : "Discard changes in";
    editor.startPrompt(`${action} "${f.path}"? This cannot be undone.`, "review-discard-confirm");
    const suggestions: PromptSuggestion[] = [
        { text: `${action} file`, description: "Permanently lose changes", value: "discard" },
        { text: "Cancel", description: "Keep the file as-is", value: "cancel" },
    ];
    editor.setPromptSuggestions(suggestions);
}
registerHandler("review_discard_file", review_discard_file);

async function on_review_discard_hunk_confirm(args: { prompt_type: string; input: string; selected_index: number | null }): Promise<boolean> {
    if (args.prompt_type !== "review-discard-hunk-confirm") return true;
    const response = args.input.trim().toLowerCase();
    if (response === "discard" || args.selected_index === 0) {
        const hunk = getHunkAtDiffCursor();
        if (hunk && hunk.file) {
            const patch = buildHunkPatch(hunk.file, hunk);
            const ok = await applyHunkPatch(patch, ["--reverse"]);
            if (ok) {
                editor.setStatus(editor.t("status.hunk_discarded") || "Hunk discarded");
                await refreshMagitData();
            }
        }
    } else {
        editor.setStatus("Discard cancelled");
    }
    return false;
}
registerHandler("on_review_discard_hunk_confirm", on_review_discard_hunk_confirm);

async function on_review_discard_confirm(args: { prompt_type: string; input: string; selected_index: number | null }): Promise<boolean> {
    if (args.prompt_type !== "review-discard-confirm") return true;

    const response = args.input.trim().toLowerCase();
    if (response === "discard" || args.selected_index === 0) {
        const f = pendingDiscardFile;
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
    pendingDiscardFile = null;
    return false;
}
registerHandler("on_review_discard_confirm", on_review_discard_confirm);

/**
 * Refresh file list and diffs using the new git status approach, then re-render.
 */
async function refreshMagitData() {
    const status = await getGitStatus();
    state.files = status.files;
    state.emptyState = status.emptyReason;
    state.hunks = await fetchDiffsForFiles(status.files);
    state.diffCursorRow = 1;
    updateMagitDisplay();
    restoreCursorAfterRebuild();
    updateReviewStatus();
}

/**
 * After a rebuild caused by stage/unstage/discard, try to land the cursor
 * back on the same hunk (now possibly in a different section), or the
 * nearest survivor in the original section, or the first hunk overall.
 */
function restoreCursorAfterRebuild() {
    const anchor = pendingHunkAnchor;
    pendingHunkAnchor = null;
    if (!anchor) return;
    if (anchor.hunkId) {
        // Find the hunk by id in the new state.
        const found = state.hunks.findIndex(h => h.id === anchor.hunkId);
        if (found >= 0) {
            // Compute its visible row (auto-expanding if needed).
            jumpToGlobalHunk(found);
            return;
        }
    }
    // Hunk vanished — fall back to the next hunk in the same section,
    // else the previous one, else the first hunk overall.
    if (anchor.section) {
        const idx = state.hunks.findIndex(h => h.gitStatus === anchor.section);
        if (idx >= 0) {
            jumpToGlobalHunk(idx);
            return;
        }
    }
    if (state.hunks.length > 0) jumpToGlobalHunk(0);
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
    // Use the file under the cursor (the file whose section the cursor is in)
    if (state.files.length === 0) return;
    const selectedFile = currentFileFromCursor();
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
    // For deleted files the path no longer exists — use empty content
    let newContent: string;
    if (selectedFile.status === 'D') {
        newContent = "";
    } else {
        const readResult = await editor.readFile(absoluteFilePath);
        if (readResult === null) {
            editor.setStatus(editor.t("status.failed_new_version"));
            return;
        }
        newContent = readResult;
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

/**
 * Move the diff panel's native cursor to the given 1-indexed row, scrolling
 * the viewport so the row is visible.
 */
function jumpDiffCursorToRow(row: number): void {
    const diffId = state.panelBuffers["diff"];
    if (diffId === undefined) return;
    const idx = row - 1;
    if (idx < 0 || idx >= state.diffLineByteOffsets.length) return;

    // Diff panel is the only scrollable panel that owns the cursor; use
    // executeAction so the normal cursor event flow fires and the status
    // bar line number updates correctly.
    if (state.focusPanel === 'diff') {
        const delta = row - state.diffCursorRow;
        const action = delta > 0 ? "move_down" : "move_up";
        for (let i = 0, n = Math.abs(delta); i < n; i++) editor.executeAction(action);
    } else {
        const byteOffset = state.diffLineByteOffsets[idx];
        editor.setBufferCursor(diffId, byteOffset);
        editor.scrollBufferToLine(diffId, idx);
    }
    state.diffCursorRow = row;
    applyCursorLineOverlay('diff');
    updateReviewStatus();
}

/**
 * Compute the 1-indexed global hunk number that corresponds to the current
 * diff-panel cursor row. Returns null when no hunk is "current".
 */
function currentGlobalHunkIndex(): number | null {
    if (state.hunkHeaderRows.length === 0) return null;
    let within = -1;
    for (let i = 0; i < state.hunkHeaderRows.length; i++) {
        if (state.hunkHeaderRows[i] <= state.diffCursorRow) within = i;
        else break;
    }
    if (within < 0) return null;
    return within + 1;
}

/**
 * Refresh the status-bar summary for review-diff mode. Shows "Hunk N of M"
 * when a current hunk is known, falls back to the bare hunk count otherwise.
 */
function updateReviewStatus(): void {
    if (state.groupId === null) return;
    const total = state.hunkHeaderRows.length;
    const current = currentGlobalHunkIndex();
    if (current !== null) {
        editor.setStatus(editor.t("status.review_summary_indexed", {
            current: String(current),
            count: String(total),
        }));
    } else {
        editor.setStatus(editor.t("status.review_summary", { count: String(total) }));
    }
}

/**
 * Find the global index in `state.hunks` of the hunk currently visible
 * at the cursor row, scanning the *visible* hunks (i.e. hunks whose
 * file is not collapsed). Returns -1 if no hunk is at or before cursor.
 */
function visibleHunkIndexAtCursor(): number {
    let visibleIdx = -1;
    for (let i = 0; i < state.hunkHeaderRows.length; i++) {
        if (state.hunkHeaderRows[i] <= state.diffCursorRow) visibleIdx = i;
        else break;
    }
    if (visibleIdx < 0) return -1;
    // Map back to the global state.hunks index.
    let visited = 0;
    for (let i = 0; i < state.hunks.length; i++) {
        const h = state.hunks[i];
        if (state.collapsedFiles.has(fileKeyOf(h.file, h.gitStatus || 'unstaged'))) continue;
        if (visited === visibleIdx) return i;
        visited++;
    }
    return -1;
}

function jumpToGlobalHunk(globalIdx: number) {
    if (globalIdx < 0 || globalIdx >= state.hunks.length) return;
    const target = state.hunks[globalIdx];
    const targetKey = fileKeyOf(target.file, target.gitStatus || 'unstaged');
    if (state.collapsedFiles.has(targetKey)) {
        // Auto-expand the file containing the target hunk before jumping.
        state.collapsedFiles.delete(targetKey);
        updateMagitDisplay();
    }
    // Find the row of the target hunk in the (now possibly rebuilt) buffer.
    let visibleIdx = 0;
    for (let i = 0; i < globalIdx; i++) {
        const h = state.hunks[i];
        if (state.collapsedFiles.has(fileKeyOf(h.file, h.gitStatus || 'unstaged'))) continue;
        visibleIdx++;
    }
    const row = state.hunkHeaderRows[visibleIdx];
    if (row !== undefined) jumpDiffCursorToRow(row);
}

function review_next_hunk() {
    if (state.groupId === null) return;
    if (state.hunks.length === 0) return;
    const cur = visibleHunkIndexAtCursor();
    // Find next hunk in global order — auto-expanding its file if needed.
    if (cur < 0) {
        jumpToGlobalHunk(0);
        return;
    }
    if (cur + 1 >= state.hunks.length) return;
    jumpToGlobalHunk(cur + 1);
}
registerHandler("review_next_hunk", review_next_hunk);

function review_prev_hunk() {
    if (state.groupId === null) return;
    if (state.hunks.length === 0) return;
    const cur = visibleHunkIndexAtCursor();
    if (cur <= 0) return;
    jumpToGlobalHunk(cur - 1);
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
    if (state.files.length === 0) return null;
    const hunk = getHunkAtDiffCursor();
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
    if (state.files.length === 0) return null;
    const cur = currentFileFromCursor();
    if (!cur) return null;

    const props = readPropsAtCursor('diff');
    const hunkId = props ? props["hunkId"] : undefined;
    if (typeof hunkId !== 'string') {
        // Fallback: first hunk for the file under the cursor.
        const hunk = state.hunks.find(
            h => h.file === cur.path && h.gitStatus === cur.category
        );
        if (!hunk) return null;
        return { hunkId: hunk.id, file: hunk.file };
    }

    const file = typeof props!["file"] === 'string' ? props!["file"] as string : cur.path;
    const lineType = props!["lineType"] as ('add' | 'remove' | 'context' | undefined);
    const oldLine = typeof props!["oldLine"] === 'number' ? props!["oldLine"] as number : undefined;
    const newLine = typeof props!["newLine"] === 'number' ? props!["newLine"] as number : undefined;
    const lineContent = typeof props!["lineContent"] === 'string' ? props!["lineContent"] as string : undefined;
    return { hunkId, file, lineType, oldLine, newLine, lineContent };
}

// Pending prompt state for event-based prompt handling
let pendingCommentInfo: PendingCommentInfo | null = null;
let editingCommentId: string | null = null; // non-null when editing an existing comment

/**
 * Find an existing comment at the current diff cursor position, either on the
 * comment display line itself or on the diff line it's attached to.
 */
function findCommentAtCursor(): ReviewComment | null {
    const props = readPropsAtCursor('diff');
    if (!props) return null;

    // Cursor sits directly on a comment display line.
    const commentId = props["commentId"];
    if (typeof commentId === 'string') {
        return state.comments.find(c => c.id === commentId) || null;
    }

    // Cursor sits on a diff line — match by hunk + line type + line number.
    const hunkId = props["hunkId"];
    const lineType = props["lineType"];
    if (typeof hunkId !== 'string') return null;
    if (lineType !== 'add' && lineType !== 'remove' && lineType !== 'context') return null;
    const oldLine = typeof props["oldLine"] === 'number' ? props["oldLine"] as number : undefined;
    const newLine = typeof props["newLine"] === 'number' ? props["newLine"] as number : undefined;
    return state.comments.find(c =>
        c.hunk_id === hunkId && (
            (c.line_type === 'add' && c.new_line === newLine) ||
            (c.line_type === 'remove' && c.old_line === oldLine) ||
            (c.line_type === 'context' && c.new_line === newLine)
        )
    ) || null;
}

async function review_add_comment() {
    const info = getCurrentLineInfo();
    if (!info) {
        editor.setStatus(editor.t("status.no_hunk_selected"));
        return;
    }

    // Check for existing comment to edit
    const existing = findCommentAtCursor();

    pendingCommentInfo = info;
    editingCommentId = existing?.id || null;

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

    const label = existing
        ? (editor.t("prompt.edit_comment", { line: lineRef }) || `Edit comment on ${lineRef}: `)
        : editor.t("prompt.comment", { line: lineRef });

    if (existing) {
        editor.startPromptWithInitial(label, "review-comment", existing.text);
    } else {
        editor.startPrompt(label, "review-comment");
    }
}
registerHandler("review_add_comment", review_add_comment);

let pendingDeleteCommentId: string | null = null;

async function review_delete_comment() {
    const target: ReviewComment | null = findCommentAtCursor();

    if (!target) {
        editor.setStatus("No comment to delete");
        return;
    }

    pendingDeleteCommentId = target.id;
    const preview = target.text.length > 40 ? target.text.substring(0, 37) + '...' : target.text;
    editor.startPrompt(`Delete "${preview}"?`, "review-delete-comment-confirm");
    const suggestions: PromptSuggestion[] = [
        { text: "Delete", description: "Remove this comment", value: "delete" },
        { text: "Cancel", description: "Keep the comment", value: "cancel" },
    ];
    editor.setPromptSuggestions(suggestions);
}
registerHandler("review_delete_comment", review_delete_comment);

function on_review_delete_comment_confirm(args: { prompt_type: string; input: string; selected_index: number | null }): boolean {
    if (args.prompt_type !== "review-delete-comment-confirm") return true;
    const response = args.input.trim().toLowerCase();
    if ((response === "delete" || args.selected_index === 0) && pendingDeleteCommentId) {
        if (pendingDeleteCommentId === '__note__') {
            state.note = '';
        } else {
            state.comments = state.comments.filter(c => c.id !== pendingDeleteCommentId);
        }
        updateMagitDisplay();
        editor.setStatus("Deleted");
    } else {
        editor.setStatus("Delete cancelled");
    }
    pendingDeleteCommentId = null;
    return false;
}
registerHandler("on_review_delete_comment_confirm", on_review_delete_comment_confirm);

// Prompt event handlers
function on_review_prompt_confirm(args: { prompt_type: string; input: string }): boolean {
    if (args.prompt_type !== "review-comment") {
        return true;
    }

    if (editingCommentId) {
        // Edit mode: update existing comment (empty text keeps the comment unchanged)
        if (args.input && args.input.trim()) {
            const existing = state.comments.find(c => c.id === editingCommentId);
            if (existing) {
                existing.text = args.input.trim();
                existing.timestamp = new Date().toISOString();
                updateMagitDisplay();
                editor.setStatus("Comment updated");
            }
        } else {
            editor.setStatus("Comment unchanged (use x to delete)");
        }
        editingCommentId = null;
        pendingCommentInfo = null;
        return true;
    }

    // New comment mode
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
        editingCommentId = null;
        editor.setStatus(editor.t("status.comment_cancelled"));
    }
    return true;
}
registerHandler("on_review_prompt_cancel", on_review_prompt_cancel);

// Register prompt event handlers
editor.on("prompt_confirmed", "on_review_prompt_confirm");
editor.on("prompt_confirmed", "on_review_discard_confirm");
editor.on("prompt_confirmed", "on_review_discard_hunk_confirm");
editor.on("prompt_confirmed", "on_review_edit_note_confirm");
editor.on("prompt_confirmed", "on_review_delete_comment_confirm");
editor.on("prompt_cancelled", "on_review_prompt_cancel");

async function review_edit_note() {
    const label = editor.t("prompt.overall_comment") || "Note: ";
    if (state.note) {
        editor.startPromptWithInitial(label, "review-edit-note", state.note);
    } else {
        editor.startPrompt(label, "review-edit-note");
    }
}
registerHandler("review_edit_note", review_edit_note);

function on_review_edit_note_confirm(args: { prompt_type: string; input: string }): boolean {
    if (args.prompt_type !== "review-edit-note") return true;
    if (args.input && args.input.trim()) {
        state.note = args.input.trim();
        updateMagitDisplay();
        editor.setStatus(state.note ? "Note saved" : "Note cleared");
    } else {
        // Empty submission: keep existing note unchanged (use x to delete)
        if (state.note) {
            editor.setStatus("Note unchanged (use x to delete)");
        }
    }
    return true;
}
registerHandler("on_review_edit_note_confirm", on_review_edit_note_confirm);

async function review_export_session() {
    const cwd = editor.getCwd();
    const reviewDir = editor.pathJoin(cwd, ".review");

    let md = `# Code Review Session\n`;
    md += `Date: ${new Date().toISOString()}\n\n`;

    if (state.note) {
        md += `## Note\n${state.note}\n\n`;
    }

    // Summary
    const filesWithComments = new Set(state.comments.map(c => c.file)).size;
    md += `## Summary\n`;
    md += `- Files: ${state.files.length}\n`;
    md += `- Hunks: ${state.hunks.length}\n`;
    if (filesWithComments > 0) {
        md += `- Files with comments: ${filesWithComments}\n`;
    }
    md += `\n`;

    // Group comments by file
    const fileComments: Record<string, ReviewComment[]> = {};
    for (const c of state.comments) {
        const file = c.file || 'unknown';
        if (!fileComments[file]) fileComments[file] = [];
        fileComments[file].push(c);
    }

    for (const [file, comments] of Object.entries(fileComments)) {
        md += `## ${file}\n\n`;
        for (const c of comments) {
            let lineRef = '';
            if (c.line_type === 'add' && c.new_line) {
                lineRef = `line +${c.new_line}`;
            } else if (c.line_type === 'remove' && c.old_line) {
                lineRef = `line -${c.old_line}`;
            } else if (c.new_line) {
                lineRef = `line ${c.new_line}`;
            } else if (c.old_line) {
                lineRef = `line ${c.old_line}`;
            }
            if (lineRef) {
                md += `- **${lineRef}**: ${c.text}\n`;
            } else {
                md += `- ${c.text}\n`;
            }
            if (c.line_content) {
                md += `  \`${c.line_content.trim()}\`\n`;
            }
        }
        md += `\n`;
    }

    const filePath = editor.pathJoin(reviewDir, "session.md");
    await editor.writeFile(filePath, md);
    editor.setStatus(editor.t("status.exported", { path: filePath }));
}
registerHandler("review_export_session", review_export_session);

async function review_export_json() {
    const cwd = editor.getCwd();
    const reviewDir = editor.pathJoin(cwd, ".review");

    const session = {
        version: "2.0",
        timestamp: new Date().toISOString(),
        note: state.note || null,
        comments: state.comments.map(c => ({
            file: c.file,
            text: c.text,
            line_type: c.line_type || null,
            old_line: c.old_line || null,
            new_line: c.new_line || null,
            line_content: c.line_content || null
        }))
    };

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
    const status = await getGitStatus();
    state.files = status.files;
    state.emptyState = status.emptyReason;
    state.hunks = await fetchDiffsForFiles(status.files);
    state.comments = [];
    state.note = '';
    state.diffCursorRow = 1;
    state.hunkHeaderRows = [];
    state.diffLineByteOffsets = [];
    state.fileHeaderRows = {};
    state.collapsedFiles = new Set();
    state.commentsByRow = {};
    state.commentsSelectedRow = 0;
    state.focusPanel = 'diff';

    // Critique-style unified layout:
    //   toolbar (2 rows fixed)
    //   ┌──── sticky file header (1 row fixed) ─┬───────────┐
    //   ├──── diff stream (scrollable) ─────────┤ comments  │
    //   └───────────────────────────────────────┴───────────┘
    const layout = JSON.stringify({
        type: "split",
        direction: "v",
        first: { type: "fixed", id: "toolbar", height: 2 },
        second: {
            type: "split",
            direction: "h",
            ratio: 0.75,
            first: {
                type: "split",
                direction: "v",
                first: { type: "fixed", id: "sticky", height: 1 },
                second: { type: "scrollable", id: "diff" },
            },
            second: { type: "scrollable", id: "comments" },
        },
    });

    const groupResult = await editor.createBufferGroup("*Review Diff*", "review-mode", layout);
    state.groupId = groupResult.groupId;
    state.panelBuffers = groupResult.panels;
    state.reviewBufferId = groupResult.panels["diff"];

    // Diff panel uses the editor's native cursor for scrolling.
    if (state.panelBuffers["diff"] !== undefined) {
        (editor as any).setBufferShowCursors(state.panelBuffers["diff"], true);
    }

    // Set initial content for all panels
    updateMagitDisplay();

    // Focus the diff panel so review-mode keybindings work immediately.
    editor.focusBufferGroupPanel(state.groupId, "diff");

    // Register resize handler
    editor.on("resize", "onReviewDiffResize");

    updateReviewStatus();
    editor.on("buffer_activated", "on_review_buffer_activated");
    editor.on("buffer_closed", "on_review_buffer_closed");
    editor.on("cursor_moved", "on_review_cursor_moved");
    editor.on("viewport_changed", "on_review_viewport_changed");
    editor.on("mouse_click", "on_review_mouse_click");
}
registerHandler("start_review_diff", start_review_diff);

function stop_review_diff() {
    if (state.groupId !== null) {
        editor.closeBufferGroup(state.groupId);
        state.groupId = null;
        state.panelBuffers = {};
    }
    state.reviewBufferId = null;
    editor.setContext("review-mode", false);
    editor.off("resize", "onReviewDiffResize");
    editor.off("buffer_activated", "on_review_buffer_activated");
    editor.off("buffer_closed", "on_review_buffer_closed");
    editor.off("cursor_moved", "on_review_cursor_moved");
    editor.off("viewport_changed", "on_review_viewport_changed");
    editor.off("mouse_click", "on_review_mouse_click");
    editor.setStatus(editor.t("status.stopped"));
}
registerHandler("stop_review_diff", stop_review_diff);


/**
 * React to a buffer becoming active. Used here purely to track which review
 * panel currently has focus (Tab and mouse clicks both fire buffer_activated).
 * The focus state drives toolbar hint rendering and the `review_nav_*`
 * handlers' files-vs-diff branching.
 *
 * Note: this used to call `refreshMagitData()` on every activation, which
 * spawned several `git` subprocesses every time the user switched panels.
 * The user has a dedicated `r` key for that — auto-refresh was too aggressive.
 */
function on_review_buffer_activated(data: { buffer_id: number }): void {
    if (state.groupId === null) return;
    const diffId = state.panelBuffers["diff"];
    const commentsId = state.panelBuffers["comments"];
    let newPanel: 'diff' | 'comments' | null = null;
    if (data.buffer_id === diffId) newPanel = 'diff';
    else if (data.buffer_id === commentsId) newPanel = 'comments';
    if (newPanel === null || newPanel === state.focusPanel) return;
    state.focusPanel = newPanel;
    // Re-render the comments panel so the selection highlight follows focus.
    editor.setPanelContent(state.groupId, "comments", buildCommentsPanelEntries());
}
registerHandler("on_review_buffer_activated", on_review_buffer_activated);

/**
 * React to native cursor movement inside review panels.
 *
 * Diff panel: keeps `state.diffCursorRow` in sync and re-paints the
 * cursor-line highlight overlay.
 *
 * Files panel: when the cursor moves (e.g. via mouse click), read the
 * `fileIndex` text property at the new position and select that file.
 * This makes click-to-select work even though the files panel hides its
 * native cursor (`show_cursors = false` blocks keyboard-driven movement
 * but mouse clicks still move the cursor).
 */
function on_review_cursor_moved(data: {
    buffer_id: number;
    cursor_id: number;
    old_position: number;
    new_position: number;
    line: number;
    text_properties: Array<Record<string, unknown>>;
}): void {
    if (state.groupId === null) return;

    // Diff panel: track cursor row + repaint the cursor-line overlay.
    if (data.buffer_id === state.panelBuffers["diff"]) {
        state.diffCursorRow = data.line;
        applyCursorLineOverlay('diff');
        updateReviewStatus();
        return;
    }
}
registerHandler("on_review_cursor_moved", on_review_cursor_moved);

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
editor.registerCommand("%cmd.edit_note", "%cmd.edit_note_desc", "review_edit_note", "review-mode");
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
    // Native cursor motion in the unified diff stream.
    ["Up", "review_nav_up"], ["Down", "review_nav_down"],
    ["k", "review_nav_up"], ["j", "review_nav_down"],
    ["PageUp", "review_page_up"], ["PageDown", "review_page_down"],
    ["Home", "review_nav_home"], ["End", "review_nav_end"],
    // Hunk navigation across the unified stream.
    ["n", "review_next_hunk"], ["p", "review_prev_hunk"],
    // Per-file collapse: Tab toggles the file under the cursor;
    // `z a` collapses every file; `z r` reveals (expands) every file.
    ["Tab", "review_toggle_file_collapse"],
    ["z a", "review_collapse_all"],
    ["z r", "review_expand_all"],
    // Visual line-selection mode for line-level stage/unstage/discard.
    ["v", "review_visual_start"],
    ["Esc", "review_visual_cancel"],
    // Drill-down to side-by-side view of the file under the cursor —
    // unless focus is in the comments panel, in which case Enter opens
    // the selected comment.
    ["Enter", "review_enter_dispatch"],
    // Comments-nav: cycle through comments, jump diff cursor, expand
    // the file if needed. Works regardless of which panel has focus.
    ["] c", "review_next_comment"],
    ["[ c", "review_prev_comment"],
    // Focus the comments panel (use j/k/Enter inside).
    ["`", "review_focus_comments"],
    // Stage/unstage/discard — context-sensitive. s/u/d act on the file
    // (when cursor is on a file header) or the hunk under the cursor.
    // Capital S/U/D always act on the enclosing file.
    ["s", "review_stage_scope"], ["u", "review_unstage_scope"],
    ["d", "review_discard_file"],
    ["S", "review_stage_file"], ["U", "review_unstage_file"],
    ["D", "review_discard_file_only"],
    ["r", "review_refresh"],
    // Comments
    ["c", "review_add_comment"],
    ["N", "review_edit_note"],
    ["x", "review_delete_comment"],
    // Close & export
    ["q", "close"],
    ["e", "review_export_session"],
], true);

editor.debug("Review Diff plugin loaded with review comments support");
